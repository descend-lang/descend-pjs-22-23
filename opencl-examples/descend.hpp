#define CL_HPP_ENABLE_EXCEPTIONS
#define BOOL_TRUE 1
#define BOOL_FALSE 0
#define CL_HPP_TARGET_OPENCL_VERSION 300
#include <CL/opencl.hpp>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <array>

const char *getErrorString(cl_int error);
std::string load_program(const std::string &input);
inline void check_opencl_err(const cl_int err, const char * const file, const int line) {
    if (err != CL_SUCCESS) {
        std::cerr << getErrorString(err) << " " << file << " " << line << std::endl;
    }
}

namespace descend {

    using i32 = cl_int;
    using u32 = cl_uint;
    using f32 = cl_float;
    // Only OpenCL 1.2+ or with extension cl_khr_fp64
    using f64 = cl_double;
    // OpenCL doesn't permit bool Kernel Params
    using boolean = cl_char;
    // Todo: As per openCL Specification it is not allowed to pass bool to a kernel

    template<typename DescendType, size_t n>
    class arr {
    private:
        std::array<DescendType,n>* content;
    public:
        static constexpr std::size_t size = sizeof(DescendType);

        arr() {
            this->content = new std::array<DescendType, n>;
        };

        ~arr(){
            delete this->content;
        }

        auto operator&() -> DescendType * {
            return this->content->data();
        }

        auto operator&() const -> const DescendType * {
            return this->content->data();
        }
    };

    template<typename DescendType, size_t n>
    using array = std::array<DescendType, n>*;

    class Gpu {
    public:
        cl::Device *device;
        cl::Context *context;
        cl::CommandQueue *queue;

        Gpu(cl::Device* device, cl::Context* context, cl::CommandQueue* queue){
            this->device = device;
            this->context = context;
            this->queue = queue;
            std::cout << "Created Gpu Device" << std::endl;
        }
        ~ Gpu () {
            std::cout << "Destroying GPU Device" << std::endl;
            this->queue->finish();
        }
    };

    enum Memory {
        CpuHeap,
        GpuGlobal
    };

    template<Memory mem, typename DescendType>
    class Buffer;

    Gpu gpu_device(std::size_t device_id) {

        std::vector<cl::Platform> platforms;
        cl::Platform::get(&platforms);
        if(platforms.empty()){
            throw std::runtime_error("No Platforms found on Computer!");
        }

        // TODO refactor
        cl_context_properties properties[] = {CL_CONTEXT_PLATFORM, (cl_context_properties) (platforms[0])(), 0};
        const auto context = new cl::Context(CL_DEVICE_TYPE_ALL);
        std::vector<cl::Device> devices = context->getInfo<CL_CONTEXT_DEVICES>();

        const auto device = new cl::Device(devices[device_id]);

        /* Check if cl_khr_fp64 (Khronos official double precision floating point precision extension)
          * is listed under extensions.
          * For AMD, maybe check for cl_amd_fp64
         */
        string device_extensions;
        device->getInfo(CL_DEVICE_EXTENSIONS, &device_extensions);
        if (!device_extensions.find("cl_khr_fp64")) {
            throw std::runtime_error("fp64 extension not installed");
        }

        cl_int err;
        // Create command queue for first device
        const auto queue = new cl::CommandQueue(*context, *device, 0, &err);

        if(err != CL_SUCCESS) {
            throw std::runtime_error(getErrorString(err));
        }

        std::cout << "Adresses (context, devices, queue)" << context <<  ", " << device << ", " << queue << std::endl;

        return Gpu(device, context, queue);
    };


    template<typename DescendType>
    class Buffer<Memory::CpuHeap, DescendType> {
        DescendType * const ptr_;

    public:
        static constexpr std::size_t size = sizeof(DescendType);

        Buffer(const DescendType init_val) : ptr_{new DescendType(init_val)} {
            std::cout << "Creating Heap Buffer of size: " << size << std::endl;
        }
        Buffer(const DescendType * const __restrict__ init_ptr) : ptr_{new DescendType(*init_ptr)} {
            std::cout << "Creating Heap Buffer of size: " << size << std::endl;
        }
        ~Buffer() {
            std::cout << "Destroying Heap Buffer of size: " << size << std::endl;
            delete ptr_;
        }

        auto operator&() -> DescendType * {
            return ptr_;
        }

        auto operator&() const -> const DescendType * {
            return ptr_;
        }
    };


    template<typename DescendType, std::size_t n>
    class Buffer<Memory::CpuHeap, descend::array<DescendType, n>> {
        descend::array<DescendType, n> * const ptr_;

    public:
        static constexpr std::size_t size = n * sizeof(DescendType);

        Buffer(const DescendType default_value): ptr_{new descend::array<DescendType, n>} {
            std::cout << "Creating Heap Buffer of size: " << size << std::endl;
            std::fill(ptr_->begin(), ptr_->end(), default_value);
        }

        Buffer(const descend::array<DescendType, n> init) : ptr_{new descend::array<DescendType, n>} {
            std::cout << "Creating Heap Buffer of size: " << size << std::endl;
            std::copy(init->begin(), init->end(), ptr_->data());
        }

        Buffer(const DescendType * const __restrict__ init_ptr) : ptr_{new descend::array<DescendType, n>} {
            std::cout << "Creating Heap Buffer of size: " << size << std::endl;
            std::copy(init_ptr, init_ptr + size, ptr_->data());
        }

        ~Buffer() {
            std::cout << "Destroying Heap Buffer of size: " << size << std::endl;
            delete ptr_;
        }

        auto operator&() -> DescendType * {
            return ptr_->data();
        }

        auto operator&() const -> const DescendType * {
            return ptr_->data();
        }

        DescendType& operator[](std::size_t idx) { return (*ptr_)[idx]; }
        const DescendType& operator[](std::size_t idx) const { return (*ptr_)[idx]; }
    };

    template<typename DescendType>
    class Buffer<Memory::GpuGlobal, DescendType> {
        const Gpu *gpu_;

    public:
        cl::Buffer* buffer;
        static constexpr std::size_t size = sizeof(DescendType);

        Buffer(const Gpu* const __restrict__ gpu, const DescendType value) : gpu_{gpu} {
            buffer = new cl::Buffer(*gpu_->context, CL_MEM_WRITE_ONLY, size);
            // Copy Data to device
            std::cout << "Create GPU Buffer of size: " << size << std::endl;
            gpu_->queue->enqueueWriteBuffer(*buffer, CL_TRUE, 0, size, *value);
        }

        Buffer(const Gpu* const __restrict__ gpu, const DescendType * const __restrict__ init_ptr) : gpu_{gpu} {
            buffer = new cl::Buffer(*gpu_->context, CL_MEM_WRITE_ONLY, size);
            // Copy Data to device
            std::cout << "Create GPU Buffer of size: " << size << std::endl;
            gpu_->queue->enqueueWriteBuffer(*buffer, CL_TRUE, 0, size, init_ptr);
        }

        template<typename PtrTypeHost>
        void read_to_host(PtrTypeHost * const __restrict__ host_ptr) const {
            gpu_->queue->enqueueReadBuffer(*buffer, CL_TRUE, 0, size, host_ptr);
        }

        ~Buffer() {
            std:: cout << "Destroying GPU Buffer of size " << size << std::endl;
            delete buffer;
        }
    };

    template<typename DescendType, std::size_t n>
    class Buffer<Memory::GpuGlobal, descend::array<DescendType, n>> {
        const Gpu *gpu_;

    public:
        cl::Buffer* buffer;
        static constexpr std::size_t size = n * sizeof(DescendType);

        Buffer(const Gpu* const __restrict__ gpu, const DescendType default_value) : gpu_{gpu} {
            auto init_ptr = new descend::array<DescendType, n>();
            std::fill(init_ptr->begin(), init_ptr->end(), default_value);
            buffer = new cl::Buffer(*gpu_->context, CL_MEM_WRITE_ONLY, size);
            // Copy Data to device
            std::cout << "Create GPU Buffer of size: " << size << std::endl;
            gpu_->queue->enqueueWriteBuffer(*buffer, CL_TRUE, 0, size, init_ptr);
        }

        Buffer(const Gpu* const __restrict__ gpu, const DescendType * const __restrict__ init_ptr) : gpu_{gpu} {
            buffer = new cl::Buffer(*gpu_->context, CL_MEM_WRITE_ONLY, size);
            // Copy Data to device
            std::cout << "Create GPU Buffer of size: " << size << std::endl;
            gpu_->queue->enqueueWriteBuffer(*buffer, CL_TRUE, 0, size, init_ptr);
        }

        Buffer(const Gpu* const __restrict__ gpu, const descend::array<DescendType, n> init) : gpu_{gpu} {
            buffer = new cl::Buffer(*gpu_->context, CL_MEM_WRITE_ONLY, size);
            // Copy Data to device
            std::cout << "Create GPU Buffer of size: " << size << std::endl;
            gpu_->queue->enqueueWriteBuffer(*buffer, CL_TRUE, 0, size, init->data());
        }

        template<typename PtrTypeHost>
        void read_to_host(PtrTypeHost * const __restrict__ host_ptr) const {
            gpu_->queue->enqueueReadBuffer(*buffer, CL_TRUE, 0, size, host_ptr);
        }

        ~Buffer() {
            std:: cout << "Destroying GPU Buffer of size " << size << std::endl;
            delete buffer;
        }
    };

    template<typename DescendType>
    using HeapBuffer = Buffer<Memory::CpuHeap, DescendType>;

    template<typename DescendType>
    using GpuBuffer = Buffer<Memory::GpuGlobal, DescendType>;

    template<typename DescendType, typename PtrType>
    GpuBuffer<DescendType> gpu_alloc_copy(const Gpu * const __restrict__ gpu, const PtrType * const __restrict__ init_ptr) {
        return descend::GpuBuffer<DescendType>(gpu, init_ptr);
    }

    template<typename DescendType, typename PtrTypeHost>
    auto copy_to_host(const GpuBuffer<DescendType>* device_buffer, PtrTypeHost * const __restrict__ host_ptr) -> void {
        device_buffer->read_to_host(host_ptr);
    }


    //cl:Buffer aufruf als Pointer in Kernel als *pointer
    template<std::size_t num_work_groups, std::size_t local_size, typename ...Args>
    void exec(const descend::Gpu * const gpu, std::string kernel_name, std::string kernel, const GpuBuffer<Args>*... args) {
        //TODO: Build Program in own function
        //TODO: Define Global Error-Handler for OpenCL (int-code handling and Exception Handling)
        cl_int err;
        cl::Program program(*gpu->context, kernel, false, &err);
        if (err != CL_SUCCESS) {
            throw std::runtime_error(getErrorString(err));
        }

        try {
            program.build(*gpu->device);

            cl::Kernel kernel(program, kernel_name.c_str(), &err);
            std::cout << "Created Kernel" << std::endl;

            cl_uint index = 0;
            ([&]
            {
                std::cout << "Set Arg " << index << "of size " << args->size << std::endl;
                kernel.setArg(index, *(args->buffer));
                index++;
            } (), ...);

            // Number of work items in each local work group
            cl::NDRange localSize(local_size);
            // Number of total work items - localSize must be devisor
            cl::NDRange globalSize((int) num_work_groups*local_size);

            std::cout << "local-size: " << local_size << " global-size: " << num_work_groups*local_size << "\n";

            // Enqueue kernel
            cl::Event event;
            gpu->queue->enqueueNDRangeKernel(
                    kernel,
                    cl::NullRange,
                    globalSize,
                    localSize,
                    NULL,
                    &event);

            // Block until kernel completion
            event.wait();

            std::cout << "Kernel Finished" << std::endl;
        }
        catch (cl::Error &e) {
            if (e.err() == CL_BUILD_PROGRAM_FAILURE) {
                // Check the build status
                std::string build_log = program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(*gpu->device);
                std::cerr << "Build Log: " << build_log << ":" << std::endl;
            } else {
                std::cerr << getErrorString(e.err()) << std::endl;
            }
        }
    }

    template<std::size_t n, typename DescendType>
    descend::array<DescendType, n> create_array() {
        return new std::array<DescendType, n>();
    }
}

const char *getErrorString(cl_int error)
{
    switch(error){
        // run-time and JIT compiler errors
        case 0: return "CL_SUCCESS";
        case -1: return "CL_DEVICE_NOT_FOUND";
        case -2: return "CL_DEVICE_NOT_AVAILABLE";
        case -3: return "CL_COMPILER_NOT_AVAILABLE";
        case -4: return "CL_MEM_OBJECT_ALLOCATION_FAILURE";
        case -5: return "CL_OUT_OF_RESOURCES";
        case -6: return "CL_OUT_OF_HOST_MEMORY";
        case -7: return "CL_PROFILING_INFO_NOT_AVAILABLE";
        case -8: return "CL_MEM_COPY_OVERLAP";
        case -9: return "CL_IMAGE_FORMAT_MISMATCH";
        case -10: return "CL_IMAGE_FORMAT_NOT_SUPPORTED";
        case -11: return "CL_BUILD_PROGRAM_FAILURE";
        case -12: return "CL_MAP_FAILURE";
        case -13: return "CL_MISALIGNED_SUB_BUFFER_OFFSET";
        case -14: return "CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST";
        case -15: return "CL_COMPILE_PROGRAM_FAILURE";
        case -16: return "CL_LINKER_NOT_AVAILABLE";
        case -17: return "CL_LINK_PROGRAM_FAILURE";
        case -18: return "CL_DEVICE_PARTITION_FAILED";
        case -19: return "CL_KERNEL_ARG_INFO_NOT_AVAILABLE";

            // compile-time errors
        case -30: return "CL_INVALID_VALUE";
        case -31: return "CL_INVALID_DEVICE_TYPE";
        case -32: return "CL_INVALID_PLATFORM";
        case -33: return "CL_INVALID_DEVICE";
        case -34: return "CL_INVALID_CONTEXT";
        case -35: return "CL_INVALID_QUEUE_PROPERTIES";
        case -36: return "CL_INVALID_COMMAND_QUEUE";
        case -37: return "CL_INVALID_HOST_PTR";
        case -38: return "CL_INVALID_MEM_OBJECT";
        case -39: return "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR";
        case -40: return "CL_INVALID_IMAGE_SIZE";
        case -41: return "CL_INVALID_SAMPLER";
        case -42: return "CL_INVALID_BINARY";
        case -43: return "CL_INVALID_BUILD_OPTIONS";
        case -44: return "CL_INVALID_PROGRAM";
        case -45: return "CL_INVALID_PROGRAM_EXECUTABLE";
        case -46: return "CL_INVALID_KERNEL_NAME";
        case -47: return "CL_INVALID_KERNEL_DEFINITION";
        case -48: return "CL_INVALID_KERNEL";
        case -49: return "CL_INVALID_ARG_INDEX";
        case -50: return "CL_INVALID_ARG_VALUE";
        case -51: return "CL_INVALID_ARG_SIZE";
        case -52: return "CL_INVALID_KERNEL_ARGS";
        case -53: return "CL_INVALID_WORK_DIMENSION";
        case -54: return "CL_INVALID_WORK_GROUP_SIZE";
        case -55: return "CL_INVALID_WORK_ITEM_SIZE";
        case -56: return "CL_INVALID_GLOBAL_OFFSET";
        case -57: return "CL_INVALID_EVENT_WAIT_LIST";
        case -58: return "CL_INVALID_EVENT";
        case -59: return "CL_INVALID_OPERATION";
        case -60: return "CL_INVALID_GL_OBJECT";
        case -61: return "CL_INVALID_BUFFER_SIZE";
        case -62: return "CL_INVALID_MIP_LEVEL";
        case -63: return "CL_INVALID_GLOBAL_WORK_SIZE";
        case -64: return "CL_INVALID_PROPERTY";
        case -65: return "CL_INVALID_IMAGE_DESCRIPTOR";
        case -66: return "CL_INVALID_COMPILER_OPTIONS";
        case -67: return "CL_INVALID_LINKER_OPTIONS";
        case -68: return "CL_INVALID_DEVICE_PARTITION_COUNT";

            // extension errors
        case -1000: return "CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR";
        case -1001: return "CL_PLATFORM_NOT_FOUND_KHR";
        case -1002: return "CL_INVALID_D3D10_DEVICE_KHR";
        case -1003: return "CL_INVALID_D3D10_RESOURCE_KHR";
        case -1004: return "CL_D3D10_RESOURCE_ALREADY_ACQUIRED_KHR";
        case -1005: return "CL_D3D10_RESOURCE_NOT_ACQUIRED_KHR";
        default: return "Unknown OpenCL error";
    }
}

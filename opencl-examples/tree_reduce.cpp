#include "descend.hpp"

const std::string KERNEL_SOURCE = R"(
#define WI 256

__kernel void __kernel__(__global int* array) {
    for(uint k = WI/2; k > 0; k = k / 2) {
        if(get_local_id(0) < k) {
            array[((get_group_id(0) * WI) + get_local_id(0))] =
                array[((get_group_id(0) * WI) + get_local_id(0))] +
                array[((get_group_id(0) * WI) + (get_local_id(0) + k))];
        }
        else {}
        barrier(CLK_GLOBAL_MEM_FENCE);
    }
}
)";

template <std::size_t wg, std::size_t wi>
auto reduce_tree(cl_int *const a_h) -> void;

int main(void)
{
    const size_t NUMBER_OF_WORK_GROUPS = 64;
    const size_t WORK_GROUP_SIZE = 256;
    const size_t DATA_SIZE = NUMBER_OF_WORK_GROUPS * WORK_GROUP_SIZE;

    cl_int array_h[DATA_SIZE];
    for(size_t i = 0; i < DATA_SIZE; i++) array_h[i] = 1;

    reduce_tree<NUMBER_OF_WORK_GROUPS, WORK_GROUP_SIZE>(array_h);
}

// wg: corresponds to gs, num of work groups
// wi: corresponds to bs, num of wis per wg
template <std::size_t wg, std::size_t wi>
auto reduce_tree(cl_int *const array_h) -> void {
    std::cout << "array_h: " << array_h[0] << std::endl;

    auto gpu = descend::gpu_device(0);
    const auto array_d = descend::gpu_alloc_copy<descend::array<descend::i32, (wg * wi)>>( gpu, array_h);

    descend::exec<wg, wi>(gpu, KERNEL_SOURCE, array_d);

    descend::copy_to_host(*array_d, array_h);
    for (std::size_t i = 0; i < wg * wi; i = i + 1) {
        std::cout << "array_h[" << i << "]: " << array_h[i] << std::endl;
    }
}

cmake_minimum_required(VERSION 3.22)
project(openCL_examples)

set(CMAKE_CXX_STANDARD 17)

add_executable(reduce_shared_mem reduce_shared_mem.cpp)

FIND_PACKAGE(OpenCL REQUIRED)

target_link_libraries(reduce_shared_mem OpenCL::OpenCL)

target_compile_definitions(reduce_shared_mem PRIVATE CL_HPP_TARGET_OPENCL_VERSION=300)
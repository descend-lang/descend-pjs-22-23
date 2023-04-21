cmake_minimum_required(VERSION 3.22)
project(openCL_examples)

set(CMAKE_CXX_STANDARD 17)

add_executable(tree_reduce tree_reduce.cpp)

FIND_PACKAGE(OpenCL REQUIRED)

target_link_libraries(tree_reduce OpenCL::OpenCL)

target_compile_definitions(tree_reduce PRIVATE CL_HPP_TARGET_OPENCL_VERSION=300)
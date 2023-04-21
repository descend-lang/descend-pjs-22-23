cmake_minimum_required(VERSION 3.22)
project(openCL_examples)

set(CMAKE_CXX_STANDARD 17)

add_executable(vector_add vector_add.cpp)

FIND_PACKAGE(OpenCL REQUIRED)

target_link_libraries(vector_add OpenCL::OpenCL)

target_compile_definitions(vector_add PRIVATE CL_HPP_TARGET_OPENCL_VERSION=300)
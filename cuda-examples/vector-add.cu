#include "descend.cuh"


template<std::size_t n>
auto inplace_vector_add(
    descend::i32 * const __restrict__ ha_array,
    const descend::i32 * const __restrict__ hb_array
) -> void {
    const auto gpu = descend::gpu(0);

    auto a_array = descend::gpu_alloc<descend::array<descend::i32, n>>(&gpu, ha_array);
    const auto b_array = descend::gpu_alloc<descend::array<descend::i32, n>>(&gpu, hb_array);

    const auto grid = descend::spawn_threads<1, 1024>(&gpu);
    descend::par_for(
        &grid,
        [] __device__ (
            descend::i32 * const __restrict__ a_array,
            const descend::i32 * const __restrict__ b_array
        ) {
            int g_tid = blockIdx.x * blockDim.x + threadIdx.x;
            a_array[g_tid] = a_array[g_tid] + b_array[g_tid];
        },
        &a_array,
        &b_array
    );

    descend::copy_to_host<descend::array<descend::i32, n>>(&a_array, ha_array);
}

auto main() -> int {
    auto ha_array = descend::HeapBuffer<descend::array<descend::i32, 1*1024>>(descend::create_array<1024, descend::i32>(0));
    const auto hb_array = descend::HeapBuffer<descend::array<descend::i32, 1*1024>>(descend::create_array<1024, descend::i32>(1));
    inplace_vector_add<1*1024>(&ha_array, &hb_array);

    for (size_t i = 0; i < 1*1024; i++) {
        if (ha_array[i] != 1) {
            std::cout << "At i = " << i << "Wrong number. Found " << ha_array[i] << " instead of 1.";
            exit(EXIT_FAILURE);
        }
    }
    exit(EXIT_SUCCESS);
}

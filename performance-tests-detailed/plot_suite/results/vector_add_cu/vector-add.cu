#define BENCH
#include "descend.cuh"

#define WG XX
#define THREADS XX

template<std::size_t n>
auto inplace_vector_add(
        descend::i32 * const ha_array,
        const descend::i32 * const hb_array
) -> void {
    const auto gpu = descend::gpu_device(0);
    auto a_array = descend::gpu_alloc_copy<descend::array<descend::i32, n>>(&gpu, &*ha_array);
    const auto b_array = descend::gpu_alloc_copy<descend::array<descend::i32, n>>(&gpu, &*hb_array);
    descend::exec<WG, THREADS>(&gpu, [] __device__ (
            descend::i32 * const p0,
            const descend::i32 * const p1) -> void {
        p0[blockIdx.x * THREADS + threadIdx.x] = p0[blockIdx.x * THREADS + threadIdx.x] + p1[blockIdx.x * THREADS + threadIdx.x];
        __syncthreads();
    }, &a_array, &b_array);
    descend::copy_to_host<descend::array<descend::i32, n>>(&a_array, ha_array);
}

descend::Benchmark benchmark{descend::BenchConfig({"inplace_vector_add"})};
auto main() -> int {
    auto ha_array = descend::HeapBuffer<descend::array<descend::i32, WG*THREADS>>(descend::create_array<WG * THREADS, descend::i32>(0));
    const auto hb_array = descend::HeapBuffer<descend::array<descend::i32, WG*THREADS>>(descend::create_array<WG * THREADS, descend::i32>(1));
    inplace_vector_add<WG*THREADS>(&ha_array, &hb_array);

    // for (size_t i = 0; i < WG*THREADS; i++) {
    //     if (ha_array[i] != 1) {
    //         std::cout << "At i = " << i << "Wrong number. Found " << ha_array[i] << " instead of 1.";
    //         exit(EXIT_FAILURE);
    //     }
    // }
    std::cout << benchmark.avg_to_csv();
    exit(EXIT_SUCCESS);
}

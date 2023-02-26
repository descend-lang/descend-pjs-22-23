#include "descend.cuh"

#define WG XX
#define THREADS XX

__device__ auto red_add(descend::i32 *const fst_half,
                        descend::i32 *const snd_half) -> void {
  *fst_half = *fst_half + *snd_half;
}

template <std::size_t n> auto reduce(descend::i32 *const ha_array) -> void {
  auto gpu = descend::gpu_device(0);
  auto a_array =
      descend::gpu_alloc_copy<descend::array<descend::i32, n>>(&gpu, &*ha_array);

  descend::exec<WG, THREADS>(
      &gpu,
      [] __device__(
                    descend::i32 *const p0) -> void {
        for (descend::i32 k = THREADS / 2; k > 0; k = k / 2) {

          if (threadIdx.x < k) {
            p0[blockIdx.x * THREADS + threadIdx.x] =
                p0[blockIdx.x * THREADS + threadIdx.x] +
                p0[blockIdx.x * THREADS + threadIdx.x + k];
          }
          __syncthreads();
        }
      },
      &a_array);
  descend::copy_to_host<descend::array<descend::i32, n>>(&a_array, ha_array);
}


auto main() -> int {
    auto ha_array = descend::HeapBuffer<descend::array<descend::i32, WG*THREADS>>(1);
    reduce<WG*THREADS>(&ha_array);

    // for (size_t i = 0; i < 64; i++) {
    //     if (h_output[i] != 1024) {
    //         std::cout << "At i = " << i << "Wrong number. Found " << h_output[i] << " instead of 1024.";
    //         exit(EXIT_FAILURE);
    //     }
    // }
    exit(EXIT_SUCCESS);
}
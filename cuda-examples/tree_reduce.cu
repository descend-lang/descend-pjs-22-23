#include "descend.cuh"
__device__ auto red_add(descend::i32 *const fst_half,
                        descend::i32 *const snd_half) -> void {
  *fst_half = *fst_half + *snd_half;
}

template <std::size_t n> auto reduce(descend::i32 *const ha_array) -> void {
  auto gpu = descend::gpu_device(0);
  auto a_array =
      descend::gpu_alloc<descend::array<descend::i32, n>>(&gpu, &*ha_array);

  descend::exec<64, 1024>(
      &gpu,
      [] __device__(descend::i32 * global_failure,
                    descend::i32 *const p0) -> void {
        if (*global_failure != -1) {
          return;
        }
        __syncthreads();
        for (descend::i32 k = 512; k > 0; k = k / 2) {

          if (threadIdx.x < k) {
            p0[blockIdx.x * 1024 + threadIdx.x] =
                p0[blockIdx.x * 1024 + threadIdx.x] +
                p0[blockIdx.x * 1024 + threadIdx.x + k];
          }
          __syncthreads();
        }
      },
      &a_array);
  descend::copy_to_host<descend::array<descend::i32, n>>(&a_array, ha_array);
}


auto main() -> int {
    auto ha_array = descend::HeapBuffer<descend::array<descend::i32, 64*1024>>(1);
    reduce<64*1024>(&ha_array);

    // for (size_t i = 0; i < 64; i++) {
    //     if (h_output[i] != 1024) {
    //         std::cout << "At i = " << i << "Wrong number. Found " << h_output[i] << " instead of 1024.";
    //         exit(EXIT_FAILURE);
    //     }
    // }
    exit(EXIT_SUCCESS);
}
#include "descend.hpp"

#define WG XX
#define THREADS XX

descend::i32 main();
void reduce65536(descend::i32 *const ha_array);
std::string kernel = R"(
#define WG XX
#define THREADS XX
__kernel void __kernel_0_65536(__global int *const p0) {
  {
    for (size_t k = 512; k > 0; k = k / 2) {
      if (get_local_id(0) < k) {
        {
          p0[((get_group_id(0) * THREADS) + (get_local_id(0) - 0))] =
              p0[((get_group_id(0) * THREADS) + (get_local_id(0) - 0))] +
              p0[((get_group_id(0) * THREADS) + ((get_local_id(0) - 0) + k))];
        }
      } else {
      }
      barrier(CLK_LOCAL_MEM_FENCE);
    }
  }
}

)";
descend::i32 main() {
    descend::array<descend::i32, WG * THREADS> a =
            descend::create_array<WG * THREADS, descend::i32>(1);
    reduce65536(a->data());
    return 0;
}

void reduce65536(descend::i32 *const ha_array) {
    descend::Gpu gpu = descend::gpu_device(0);
    descend::GpuBuffer<descend::array<descend::i32, WG * THREADS>> a_array =
            descend::gpu_alloc_copy<descend::array<descend::i32, WG * THREADS>>(
                    (&gpu), (&(*ha_array)));
    descend::exec<WG, THREADS>((&gpu), "__kernel_0_65536", kernel, (&a_array));
    descend::copy_to_host<descend::array<descend::i32, WG * THREADS>>((&a_array),
                                                               ha_array);

    // for(int i = 0; i < WG * THREADS; i++) {
    //     std::cout << ha_array[i] << std::endl;
    // }

}
#include "descend.hpp"

#define WG 64
#define THREADS 256

descend::i32 main();
void inplace_vector_add(descend::i32 *const ha_array,
                        const descend::i32 *const hb_array);
std::string kernel = R"(
#define WG 64
#define THREADS 256
__kernel void __kernel_0_(__global int *const p0,
                          __global const int *const p1) {
  {
    {
      p0[((get_group_id(0) * THREADS) + get_local_id(0))] =
          p0[((get_group_id(0) * THREADS) + get_local_id(0))] +
          p1[((get_group_id(0) * THREADS) + get_local_id(0))];
    }
  }
}

)";
descend::i32 main() {
  descend::array<descend::i32, WG * THREADS> a =
      descend::create_array<WG * THREADS, descend::i32>(1);
  const descend::array<descend::i32, WG * THREADS> result =
      descend::create_array<WG * THREADS, descend::i32>(1);
  inplace_vector_add(a->data(), result->data());
  return 0;
}

void inplace_vector_add(descend::i32 *const ha_array,
                        const descend::i32 *const hb_array) {
  descend::Gpu gpu = descend::gpu_device(0);
  descend::GpuBuffer<descend::array<descend::i32, WG * THREADS>> a_array =
      descend::gpu_alloc_copy<descend::array<descend::i32, WG * THREADS>>(
          (&gpu), (&(*ha_array)));
  const descend::GpuBuffer<descend::array<descend::i32, WG * THREADS>> b_array =
      descend::gpu_alloc_copy<descend::array<descend::i32, WG * THREADS>>(
          (&gpu), (&(*hb_array)));
  descend::exec<WG, THREADS>((&gpu), "__kernel_0_", kernel, (&a_array),
                          (&b_array));
  descend::copy_to_host<descend::array<descend::i32, WG * THREADS>>((&a_array),
                                                             ha_array);
}
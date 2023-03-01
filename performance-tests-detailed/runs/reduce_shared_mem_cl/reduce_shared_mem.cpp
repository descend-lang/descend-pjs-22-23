#include "descend.hpp"

#define WG 4
#define THREADS 16

descend::i32 main();
descend::i32 reduce_shared_mem1004(const descend::i32 *const ha_array,
                                   descend::i32 *const h_output);
std::string kernel = R"(
#define WG 4
#define THREADS 16
__kernel void __kernel_0_1004(__global const int *const p0,
                              __global int *const p1) {
  __local int tmp[4];
  {
    { tmp[get_local_id(0)] = p0[((get_group_id(0) * 4) + get_local_id(0))]; }
    for (size_t k = (THREADS / 2); k > 0; k = k / 2) {
      if (get_local_id(0) < k) {
        tmp[(get_local_id(0) - 0)] =
            tmp[(get_local_id(0) - 0)] + tmp[((get_local_id(0) - 0) + k)];
      } else {
      }
      barrier(CLK_LOCAL_MEM_FENCE);
    }
    if (get_local_id(0) < 1) {
      p1[((get_group_id(0) * 1) + (get_local_id(0) - 0))] =
          tmp[(get_local_id(0) - 0)];
    } else {
    }
    barrier(CLK_LOCAL_MEM_FENCE);
  }
}

)";
descend::i32 main() {
  const descend::array<descend::i32, WG * THREADS> a =
      descend::create_array<WG * THREADS, descend::i32>(0);
  descend::array<descend::i32, WG> result =
      descend::create_array<WG, descend::i32>(0);
  reduce_shared_mem1004(a->data(), result->data());
  return 0;
}

descend::i32 reduce_shared_mem1004(const descend::i32 *const ha_array,
                                   descend::i32 *const h_output) {
  descend::Gpu gpu = descend::gpu_device(0);
  const descend::GpuBuffer<descend::array<descend::i32, (WG * THREADS)>> a_array =
      descend::gpu_alloc_copy<descend::array<descend::i32, (WG * THREADS)>>(
          (&gpu), ha_array);
  descend::GpuBuffer<descend::array<descend::i32, WG>> out_array =
      descend::gpu_alloc_copy<descend::array<descend::i32, WG>>(
          (&gpu), (&(*h_output)));
  descend::exec<WG, THREADS>((&gpu), "__kernel_0_1004", kernel, (&a_array),
                        (&out_array));
  descend::copy_to_host<descend::array<descend::i32, WG>> ((&out_array),
                                                           (h_output));
  int res = 0;
  for (size_t $i___25 = 0; $i___25 < WG; $i___25 = $i___25 + 1) {
    res = res + h_output[$i___25];
  }
  std::cout << "res = " << res << std::endl;
  return res;
}

// descend::i32 main();
// void reduce65536(descend::i32 *const ha_array);
// std::string kernel = R"(
// #define WG XX
// #define THREADS XX
// __kernel void __kernel_0_65536(__global int *const p0) {
//   {
//     for (size_t k = THREADS / 2; k > 0; k = k / 2) {
//       if (get_local_id(0) < k) {
//         {
//           p0[((get_group_id(0) * THREADS) + (get_local_id(0) - 0))] =
//               p0[((get_group_id(0) * THREADS) + (get_local_id(0) - 0))] +
//               p0[((get_group_id(0) * THREADS) + ((get_local_id(0) - 0) + k))];
//         }
//       } else {
//       }
//       barrier(CLK_LOCAL_MEM_FENCE);
//     }
//   }
// }

// )";
// descend::i32 main() {
//     descend::array<descend::i32, WG * THREADS> a =
//             descend::create_array<WG * THREADS, descend::i32>(1);
//     reduce65536(a->data());
//     return 0;
// }

// void reduce65536(descend::i32 *const ha_array) {
//     descend::Gpu gpu = descend::gpu_device(0);
//     descend::GpuBuffer<descend::array<descend::i32, WG * THREADS>> a_array =
//             descend::gpu_alloc_copy<descend::array<descend::i32, WG * THREADS>>(
//                     (&gpu), (&(*ha_array)));
//     descend::exec<WG, THREADS>((&gpu), "__kernel_0_65536", kernel, (&a_array));
//     descend::copy_to_host<descend::array<descend::i32, WG * THREADS>>((&a_array),
//                                                                ha_array);

//     // for(int i = 0; i < WG * THREADS; i++) {
//     //     std::cout << ha_array[i] << std::endl;
//     // }

// }
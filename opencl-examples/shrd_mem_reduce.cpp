#include "descend.hpp"
#define WG 4
#define WI 2

const std::string kernel_source = R"(
#define WG 4
#define WI 2

__kernel void __kernel__(__global const int *const a, __global int *const out) {

    int id_local = get_local_id(0);
    int id_group = get_group_id(0);
    int id_global = get_global_id(0);
    __local int tmp[WI];

    {
        {tmp[id_local] = a[id_global];}

        barrier(CLK_LOCAL_MEM_FENCE);

        for (size_t k = (WI / 2); k > 0; k = k / 2 ) {
            if (id_local < k) {
                tmp[id_local] = tmp[id_local] + tmp[id_local + k];
            }
        }
        barrier(CLK_LOCAL_MEM_FENCE);

        if (id_local < 1) {
            out[id_group] = tmp[id_local];
        }
    }
}
)";

template <std::size_t wg, std::size_t wi>
auto reduce_shared_mem(cl_int *const a_h, cl_int *const out_h) -> void;

int main(void)
{
    // could be nicer
    cl_int a_h[] = {1, 2, 3, 4, 5, 6, 7, 8};
    cl_int out_h[] = {0, 0, 0, 0};
    
    reduce_shared_mem<WG, WI>(a_h, out_h);

    cl_int res = 0;
    for (int i = 0; i < WG*WI; i++) {
        res += a_h[i];
    }
    std::cout << "result should be: " << res << std::endl;

    
    return 0;
}

// wg: corresponds to gs, num of work groups 
// wi: corresponds to bs, num of wis per wg  
template <std::size_t wg, std::size_t wi>
auto reduce_shared_mem(cl_int *const a_h, cl_int *const out_h) -> void {

    std::cout << "a_h: " << a_h[0] << ", out_h: " << out_h[0] << std::endl;

    auto gpu = descend::gpu_device(0);

    const auto a_d = descend::gpu_alloc_copy<descend::array<descend::i32, (wg * wi)>>( gpu, a_h);
    const auto out_d = descend::gpu_alloc_copy<descend::array<descend::i32, wg>>( gpu, out_h);

    descend::exec<wg, wi>(gpu, kernel_source, a_d, out_d);

    descend::copy_to_host(*out_d, out_h);

    auto sol = 0;
    for (std::size_t i = 0; i < wg; i = i + 1) {
        std::cout << "out_h: " << out_h[i] << std::endl;
        sol = sol + out_h[i];
    }

    std::cout << "computed result: " << sol << std::endl;

}

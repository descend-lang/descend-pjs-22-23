//
// Created by Justus Schneider on 21.01.23.
//

#include "descend.hpp"

std::string kernel = R"(
)";
descend::i32 reduce_shared_mem(const descend::i32 *const ha_array,
                               descend::i32 *const h_output, std::size_t gs,
                               std::size_t bs) {
    {
        auto gpu = descend::gpu_device(0);
        const auto a_array =
                descend::gpu_alloc_copy<descend::array<descend::i32, (gs * bs)>>(
                        (&gpu), ha_array);
        auto out_array = descend::gpu_alloc_copy<descend::array<descend::i32, gs>>(
                (&gpu), (&(*h_output)));
        descend::copy_to_host((&out_array), (&(*h_output)));
        auto res = 0;
        for (std::size_t $i___11 = 0; $i___11 < gs; $i___11 = $i___11 + 1) {
            res = res + h_output[$i___11];
        }
        return res;
    }
}

void no_templ_params() {
    {
        const auto a = descend::create_array<100, descend::i32>(0);
        descend::array<descend::i32, 100> result =
                descend::create_array<100, descend::i32>(0);
        reduce_shared_mem((&a), (&result), 100, 4);
    }
}
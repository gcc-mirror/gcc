// { dg-options "-fshort-enums" }
// { dg-do compile { target c++11 } }

// Bug 89624 HLE bits don't work with -fshort-enums or -fstrict-enums

#include <atomic>

static_assert((std::memory_order_acquire | std::__memory_order_hle_acquire)
    != std::memory_order_acquire, "HLE acquire sets a bit");

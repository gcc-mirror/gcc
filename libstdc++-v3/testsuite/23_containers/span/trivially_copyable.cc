// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p2251r1.pdf

#include <span>

static_assert( std::is_trivially_copyable_v<std::span<int>> );
static_assert( std::is_trivially_copyable_v<std::span<int, 42>> );

struct NonTrivial { NonTrivial(); NonTrivial(const NonTrivial&); };
static_assert( std::is_trivially_copyable_v<std::span<NonTrivial>> );
static_assert( std::is_trivially_copyable_v<std::span<NonTrivial, 99>> );

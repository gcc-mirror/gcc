// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <bit>

template<typename T> constexpr bool is_int = false;
template<> constexpr bool is_int<int> = true;

// LWG 3656. Inconsistent bit operations returning a count
// Rturn type of std::bit_width(T) changed from T to int.
static_assert( is_int<decltype(std::bit_width(1u))> );
static_assert( is_int<decltype(std::bit_width(1ul))> );
static_assert( is_int<decltype(std::bit_width(1ull))> );
static_assert( is_int<decltype(std::bit_width((unsigned short)1))> );
static_assert( is_int<decltype(std::bit_width((unsigned char)1))> );

// { dg-do compile { target c++11 } }
// { dg-require-cstdint "" }

#include <random>

std::mt19937 urng;

std::__detail::_Adaptor<std::mt19937, unsigned long> aurng(urng);

auto x = std::generate_canonical<std::size_t,
			std::numeric_limits<std::size_t>::digits>(urng);

// { dg-error "static assertion failed: template argument must be a floating point type" "" { target *-*-* } 156 }

// { dg-error "static assertion failed: template argument must be a floating point type" "" { target *-*-* } 3320 }

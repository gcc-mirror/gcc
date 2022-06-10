// { dg-do compile { target c++17 } }
#include <numeric>
#include <climits>

// PR libstdc++/105844

// |INT_MIN| can be represented in common_type_t<int, unsigned> i.e. unsigned.
static_assert( std::lcm(INT_MIN, 1u) == INT_MAX+1u );
static_assert( std::lcm(1u, INT_MIN) == INT_MAX+1u );

// But |INT_MIN| cannot be represented in common_type<int, int> i.e. int.
constexpr int a = std::lcm(INT_MIN, 1); // { dg-error "overflow" }
constexpr int b = std::lcm(1, INT_MIN); // { dg-error "overflow" }

// And the LCM of 50000 and 49999 cannot be represented in int.
constexpr int c = std::lcm(50000, 49999); // { dg-error "overflow" }
constexpr int d = std::lcm(49999, 50000); // { dg-error "overflow" }

// Similarly for unsigned, but the diagnostic is a failed assertion instead.
constexpr int e = std::lcm(500000u, 499999); // { dg-error "in 'constexpr'" }
constexpr int f = std::lcm(499999u, 500000); // { dg-error "in 'constexpr'" }
// { dg-error "unreachable" "" { target *-*-* } 0 }

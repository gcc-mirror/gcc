// { dg-do compile { target c++17 } }
#include <numeric>
#include <climits>

// PR libstdc++/105844

// |INT_MIN| can be represented in common_type_t<int, unsigned> i.e. unsigned.
static_assert( std::gcd(INT_MIN, 2u) == 2 );
static_assert( std::gcd(2u, INT_MIN) == 2 );

// |LLONG_MIN| can be represented in unsigned long long.
static_assert( std::gcd(LLONG_MIN, 2ull) == 2 );
static_assert( std::gcd(2ull, LLONG_MIN) == 2 );

// But |INT_MIN| cannot be represented in common_type<int, int> i.e. int.
constexpr int a = std::gcd(INT_MIN, 1); // { dg-error "in .constexpr." }
constexpr int b = std::gcd(1, INT_MIN); // { dg-error "in .constexpr." }

// And |LLONG_MIN| cannot be represented in long.
constexpr long long c = std::gcd(LLONG_MIN, 1); // { dg-error "in .constexpr." }
constexpr long long d = std::gcd(1, LLONG_MIN); // { dg-error "in .constexpr." }

// { dg-error "overflow" "" { target *-*-* } 0 }

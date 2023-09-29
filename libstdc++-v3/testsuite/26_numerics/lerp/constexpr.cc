// { dg-do compile { target c++20 } }

#include <cmath>

// Fails template argument deduction unless both arguments are the same type.
template<typename T>
constexpr bool
eq(T result, T expected) { return result == expected; }

static_assert( eq( std::lerp(-10.0, 10.0, 0.25), -5.0 ) );
static_assert( eq( std::lerp(2.0f, 2.0f, 200.0f), 2.0f ) );
static_assert( eq( std::lerp(2.0L, 4.0L, 200.0L), 402.0L ) );
// at least one type is long double, so result is long double
static_assert( eq( std::lerp(2.0L, 4.0f, -20.0), -38.0L ) );
// at least one type is double, so result is double:
static_assert( eq( std::lerp(-8.0f, 10.0, 0.5f), 1.0 ) );
// int promotes to double, so result is double
static_assert( eq( std::lerp(0, 1, 0), 0.0 ) );
// int promotes to double, so result is double
static_assert( eq( std::lerp(2.0f, -10.0f, 1), -10.0 ) );

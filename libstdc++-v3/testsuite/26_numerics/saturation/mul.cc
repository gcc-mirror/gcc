// { dg-do compile { target c++26 } }

// C++26 Saturation arithmetic [numerics.sat]

#include <numeric>
#include <climits>

template<typename T, typename U>
concept can_mul_sat
  = requires(T t, U u) { { std::mul_sat(t, u) } -> std::same_as<T>; };

static_assert( can_mul_sat<int, int> );
static_assert( not can_mul_sat<int, short> );
static_assert( not can_mul_sat<unsigned, int> );
static_assert( noexcept(std::mul_sat(0, 0)) );

using std::mul_sat;

static_assert(mul_sat(1, 1) == 1);
static_assert(mul_sat(10, 11) == 110);
static_assert(mul_sat(INT_MAX / 2, 3) == INT_MAX);
static_assert(mul_sat(INT_MAX / 2, -3) == INT_MIN);
static_assert(mul_sat(INT_MAX / -2, 3) == INT_MIN);
static_assert(mul_sat(INT_MIN / 2, -3) == INT_MAX);
static_assert(mul_sat(INT_MIN, -1) == INT_MAX);
static_assert(mul_sat(INT_MAX, -1) == INT_MIN + 1);
static_assert(mul_sat(INT_MAX, INT_MAX) == INT_MAX);
static_assert(mul_sat(INT_MAX, -INT_MAX) == INT_MIN);
static_assert(mul_sat(UINT_MAX, UINT_MAX) == UINT_MAX);
static_assert(mul_sat(UINT_MAX, 0u) == 0);
static_assert(mul_sat(0u, UINT_MAX) == 0);
static_assert(mul_sat((short)SHRT_MAX, (short)2) == SHRT_MAX);
static_assert(mul_sat((short)SHRT_MAX, (short)SHRT_MIN) == SHRT_MIN);
static_assert(mul_sat<long long>(SHRT_MAX, 2) == 2L * SHRT_MAX);

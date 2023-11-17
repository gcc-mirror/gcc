// { dg-do compile { target c++26 } }

// C++26 Saturation arithmetic [numerics.sat]

#include <numeric>
#include <climits>

template<typename T, typename U>
concept can_div_sat
  = requires(T t, U u) { { std::div_sat(t, u) } -> std::same_as<T>; };

static_assert( can_div_sat<int, int> );
static_assert( not can_div_sat<int, short> );
static_assert( not can_div_sat<unsigned, int> );
static_assert( noexcept(std::div_sat(0, 1)) );

using std::div_sat;

static_assert(std::div_sat(0, 1) == 0);
static_assert(std::div_sat(0, -1) == 0);
static_assert(std::div_sat(1, -1) == -1);
static_assert(std::div_sat(10, -2) == -5);
static_assert(std::div_sat(-10, -2) == 5);
static_assert(std::div_sat(INT_MAX, 1) == INT_MAX);
static_assert(std::div_sat(INT_MIN, 1) == INT_MIN);
static_assert(std::div_sat(INT_MIN + 1, -1) == INT_MAX);
static_assert(std::div_sat(0u, 1u) == 0u);
static_assert(std::div_sat(UINT_MAX, 1u) == UINT_MAX);
static_assert(std::div_sat(INT_MIN, -1) == INT_MAX);
static_assert(std::div_sat((short)SHRT_MIN, (short)-1) == SHRT_MAX);
static_assert(std::div_sat(LONG_MIN, -1L) == LONG_MAX);
static_assert(std::div_sat(LLONG_MIN, -1LL) == LLONG_MAX);

template<auto N>
std::integral_constant<decltype(N), std::div_sat(N, N-N)>
div_sat_by_zero();

template<auto N>
concept can_div_sat_by_zero = requires { div_sat_by_zero<N>(); };

static_assert( not can_div_sat_by_zero<0> );
static_assert( not can_div_sat_by_zero<1> );
static_assert( not can_div_sat_by_zero<1u> );
static_assert( not can_div_sat_by_zero<-1L> );
static_assert( not can_div_sat_by_zero<short(99)> );

// { dg-do compile { target c++26 } }

// C++26 Saturation arithmetic [numerics.sat]

#include <numeric>
#include <limits>

template<typename T, typename U>
concept can_sub_sat
  = requires(T t, U u) { { std::sub_sat(t, u) } -> std::same_as<T>; };

static_assert( can_sub_sat<int, int> );
static_assert( not can_sub_sat<int, short> );
static_assert( not can_sub_sat<unsigned, int> );
static_assert( noexcept(std::sub_sat(0, 0)) );

using std::sub_sat;

// Signed type
static_assert(sub_sat(0, 0) == 0);
static_assert(sub_sat(1, 1) == 0);
static_assert(sub_sat(-1, -1) == 0);
static_assert(sub_sat(-1, 1) == -2);
constexpr auto max = std::numeric_limits<int>::max();
constexpr auto min = std::numeric_limits<int>::min();
static_assert(sub_sat(max, 1) == max - 1);
static_assert(sub_sat(1, max) == 1 - max);
static_assert(sub_sat(max, max) == 0);
static_assert(sub_sat(min, 1) == min);
static_assert(sub_sat(min, 123) == min);
static_assert(sub_sat(0, max) == min + 1);
static_assert(sub_sat(-1, max) == min);
static_assert(sub_sat(-2, max) == min);
static_assert(sub_sat(-2, min) == max - 1);
static_assert(sub_sat(-1, min) == max);
static_assert(sub_sat(0, min) == max);
static_assert(sub_sat(1, min) == max);
static_assert(sub_sat(min, -1) == min + 1);
static_assert(sub_sat(min, min) == 0);
static_assert(sub_sat(max, min) == max);
static_assert(sub_sat(min, max) == min);

// Wider signed type than the args
static_assert(sub_sat<long long>(max, min) == (long long)max * 2 + 1);
static_assert(sub_sat<long long>(min, max) == (long long)min * 2 + 1);

// Signed type that undergoes integer promotion
constexpr auto shrt_max = std::numeric_limits<short>::max();
constexpr auto shrt_min = std::numeric_limits<short>::min();
static_assert(sub_sat<short>(0, 0) == 0);
static_assert(sub_sat<short>(1, 1) == 0);
static_assert(sub_sat<short>(3, 1) == 2);
static_assert(sub_sat<short>(shrt_max, shrt_max) == 0);
static_assert(sub_sat<short>(shrt_max, 1) == shrt_max - 1);
static_assert(sub_sat<short>(1, shrt_max) == shrt_min + 2);
static_assert(sub_sat<short>(shrt_max, shrt_min) == shrt_max);
static_assert(sub_sat<short>(0, shrt_min) == shrt_max);
static_assert(sub_sat<short>(shrt_min, (short)1) == shrt_min);
static_assert(sub_sat<short>(shrt_min, (short)-1) == shrt_min + 1);
static_assert(sub_sat<short>((short)-1, shrt_min) == shrt_max);
static_assert(sub_sat<short>((short)1, shrt_min) == shrt_max);

// Unsigned type
static_assert(sub_sat(0u, 0u) == 0u);
static_assert(sub_sat(1u, 1u) == 0u);
static_assert(sub_sat(-1u, -1u) == 0u);
static_assert(sub_sat(-1u, 1u) == -2u);
constexpr auto umax = std::numeric_limits<unsigned>::max();
static_assert(sub_sat(0u, 1u) == 0u);
static_assert(sub_sat(umax, umax) == 0u);
static_assert(sub_sat(umax, 0u) == umax);
static_assert(sub_sat(0u, umax) == 0u);
static_assert(sub_sat(umax, 1u) == umax - 1u);
static_assert(sub_sat(0u, 0u) == 0u);

// Wider unsigned type than the args
static_assert(sub_sat<unsigned long long>(0u, umax) == 0u);

// Unsigned type that undergoes integer promotion
constexpr auto ushrt_max = std::numeric_limits<unsigned short>::max();
static_assert(sub_sat<unsigned short>(0, 0) == 0);
static_assert(sub_sat<unsigned short>(1, 1) == 0);
static_assert(sub_sat<unsigned short>(3, 1) == 2);
static_assert(sub_sat<unsigned short>(ushrt_max, ushrt_max) == 0);
static_assert(sub_sat<unsigned short>(0, 1) == 0);
static_assert(sub_sat<unsigned short>(1, ushrt_max) == 0);

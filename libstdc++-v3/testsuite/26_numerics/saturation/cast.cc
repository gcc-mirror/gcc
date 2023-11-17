// { dg-do compile { target c++26 } }

// C++26 Saturation arithmetic [numerics.sat]

#include <numeric>
#include <climits>

#if CHAR_BIT == 8
static_assert(std::saturate_cast<unsigned char>(999) == 255);
static_assert(std::saturate_cast<signed char>(999) == 127);
#endif
static_assert(std::saturate_cast<unsigned short>(999) == 999);
static_assert(std::saturate_cast<signed short>(999) == 999);
static_assert(std::saturate_cast<short>(INT_MAX) == SHRT_MAX);
static_assert(std::saturate_cast<short>(UINT_MAX) == SHRT_MAX);
static_assert(std::saturate_cast<short>(UINT_MAX) == SHRT_MAX);
static_assert(std::saturate_cast<unsigned short>(UINT_MAX) == USHRT_MAX);
static_assert(std::saturate_cast<int>(UINT_MAX) == INT_MAX);
static_assert(std::saturate_cast<int>(INT_MAX) == INT_MAX);
static_assert(std::saturate_cast<unsigned>(-1) == 0);
static_assert(std::saturate_cast<unsigned>(INT_MIN) == 0);
static_assert(std::saturate_cast<unsigned>(UINT_MAX) == UINT_MAX);
static_assert(std::saturate_cast<unsigned>(LLONG_MAX) == UINT_MAX);
static_assert(std::saturate_cast<unsigned>(ULLONG_MAX) == UINT_MAX);

// { dg-do run { target c++26 } }

#include <stdbit.h>

#if __STDC_VERSION_STDBIT_H__ != 202311L
# error "__STDC_VERSION_STDBIT_H__ not defined correctly in <stdbit.h>"
#endif
#ifndef __STDC_ENDIAN_BIG__
# error "__STDC_ENDIAN_BIG__ is not defined in <stdbit.h>"
#endif
#ifndef __STDC_ENDIAN_LITTLE__
# error "__STDC_ENDIAN_LITTLE__ is not defined in <stdbit.h>"
#endif
#ifndef __STDC_ENDIAN_NATIVE__
# error "__STDC_ENDIAN_NATIVE__ is not defined in <stdbit.h>"
#endif

#include <testsuite_hooks.h>
#include <climits>

void
test_leading_zeros()
{
  VERIFY( stdc_leading_zeros_uc(0) == CHAR_BIT );
  VERIFY( stdc_leading_zeros_uc(UCHAR_MAX) == 0 );
  VERIFY( stdc_leading_zeros_uc(CHAR_MAX) == (CHAR_MIN ? 1 : 0) );
  VERIFY( stdc_leading_zeros_uc(UCHAR_MAX >> 3) == 3 );
  VERIFY( stdc_leading_zeros_uc((unsigned char)(UCHAR_MAX << 3)) == 0 );

  // TODO: replace __X_WIDTH__ with the standard macros once they're in C++26
  VERIFY( stdc_leading_zeros_us(0) == __SHRT_WIDTH__ );
  VERIFY( stdc_leading_zeros_us(USHRT_MAX) == 0 );
  VERIFY( stdc_leading_zeros_us(USHRT_MAX >> 11) == 11 );

  VERIFY( stdc_leading_zeros_ui(0) == __INT_WIDTH__ );
  VERIFY( stdc_leading_zeros_ui(UINT_MAX) == 0 );
  VERIFY( stdc_leading_zeros_ui(UINT_MAX >> 11) == 11 );

  VERIFY( stdc_leading_zeros_ul(0) == __LONG_WIDTH__ );
  VERIFY( stdc_leading_zeros_ul(ULONG_MAX) == 0 );
  VERIFY( stdc_leading_zeros_ul(ULONG_MAX >> 19) == 19 );

  VERIFY( stdc_leading_zeros_ull(0) == __LONG_LONG_WIDTH__ );
  VERIFY( stdc_leading_zeros_ull(ULLONG_MAX) == 0 );
  VERIFY( stdc_leading_zeros_ull(ULLONG_MAX >> 33) == 33 );
  VERIFY( stdc_leading_zeros_ull(7) == (__LONG_LONG_WIDTH__ - 3) );

  VERIFY( stdc_leading_zeros(7U) == (__INT_WIDTH__ - 3) );
  VERIFY( stdc_leading_zeros(7UL) == (__LONG_WIDTH__ - 3) );
  VERIFY( stdc_leading_zeros(7ULL) == (__LONG_LONG_WIDTH__ - 3) );

  static_assert( std::is_same_v<decltype(stdc_leading_zeros(0UL)), unsigned> );
}

void
test_leading_ones()
{
  VERIFY( stdc_leading_ones_uc(0) == 0 );
  VERIFY( stdc_leading_ones_uc(UCHAR_MAX) == CHAR_BIT );
  VERIFY( stdc_leading_ones_uc(CHAR_MAX) == (CHAR_MIN ? 0 : CHAR_BIT) );
  VERIFY( stdc_leading_ones_uc(UCHAR_MAX >> 3) == 0 );
  VERIFY( stdc_leading_ones_uc((unsigned char)(UCHAR_MAX << 3)) == (CHAR_BIT - 3) );

  VERIFY( stdc_leading_ones_us(0) == 0 );
  VERIFY( stdc_leading_ones_us(USHRT_MAX) == __SHRT_WIDTH__ );
  VERIFY( stdc_leading_ones_us((unsigned short)(USHRT_MAX << 11)) == (__SHRT_WIDTH__ - 11) );
  VERIFY( stdc_leading_ones_us(USHRT_MAX >> 11) == 0 );

  VERIFY( stdc_leading_ones_ui(0) == 0 );
  VERIFY( stdc_leading_ones_ui(UINT_MAX) == __INT_WIDTH__ );
  VERIFY( stdc_leading_ones_ui(UINT_MAX << 11) == (__INT_WIDTH__ - 11) );

  VERIFY( stdc_leading_ones_ul(0) == 0 );
  VERIFY( stdc_leading_ones_ul(ULONG_MAX) == __LONG_WIDTH__ );
  VERIFY( stdc_leading_ones_ul(ULONG_MAX << 19) == (__LONG_WIDTH__ - 19) );

  VERIFY( stdc_leading_ones_ull(0) == 0 );
  VERIFY( stdc_leading_ones_ull(ULLONG_MAX) == __LONG_LONG_WIDTH__ );
  VERIFY( stdc_leading_ones_ull(ULLONG_MAX << 33) == (__LONG_LONG_WIDTH__ - 33) );

  VERIFY( stdc_leading_ones(-1U << 2) == (__INT_WIDTH__ - 2) );
  VERIFY( stdc_leading_ones(-1UL << 3) == (__LONG_WIDTH__ - 3) );
  VERIFY( stdc_leading_ones(-1ULL << 4) == (__LONG_LONG_WIDTH__ - 4) );

  static_assert( std::is_same_v<decltype(stdc_leading_ones(0UL)), unsigned> );
}

void
test_trailing_zeros()
{
  VERIFY( stdc_trailing_zeros_uc((unsigned char)(UCHAR_MAX << 5)) == 5 );
  VERIFY( stdc_trailing_zeros_us((unsigned short)(USHRT_MAX << 5)) == 5 );
  VERIFY( stdc_trailing_zeros_ui(UINT_MAX << 9) == 9 );
  VERIFY( stdc_trailing_zeros_ul(ULONG_MAX << 19) == 19 );
  VERIFY( stdc_trailing_zeros_ull(ULLONG_MAX << 39) == 39 );
  VERIFY( stdc_trailing_zeros(8U) == 3 );

  static_assert( std::is_same_v<decltype(stdc_trailing_zeros(0UL)), unsigned> );
}

void
test_trailing_ones()
{
  VERIFY( stdc_trailing_ones_uc((unsigned char)3) == 2 );
  VERIFY( stdc_trailing_ones_uc((unsigned char)135) == 3 );
  VERIFY( stdc_trailing_ones_us((unsigned short)7) == 3 );
  VERIFY( stdc_trailing_ones_us((unsigned short)23) == 3 );
  VERIFY( stdc_trailing_ones_us((unsigned short)235) == 2 );
  VERIFY( stdc_trailing_ones_ui(11U) == 2 );
  VERIFY( stdc_trailing_ones_ui(15U) == 4 );
  VERIFY( stdc_trailing_ones_ul(ULONG_MAX) == __LONG_WIDTH__ );
  VERIFY( stdc_trailing_ones_ull(ULLONG_MAX) == __LONG_LONG_WIDTH__ );
  VERIFY( stdc_trailing_ones(7U) == 3 );

  static_assert( std::is_same_v<decltype(stdc_trailing_ones(0UL)), unsigned> );
}

void
test_first_leading_zero()
{
  VERIFY( stdc_first_leading_zero_uc(0) == 1 );
  VERIFY( stdc_first_leading_zero_uc(UCHAR_MAX) == 0 );
  VERIFY( stdc_first_leading_zero_uc(UCHAR_MAX ^ 0b111) == (CHAR_BIT - 2) );
  VERIFY( stdc_first_leading_zero_us(USHRT_MAX) == 0 );
  VERIFY( stdc_first_leading_zero_us(USHRT_MAX ^ 0b111) == (__SHRT_WIDTH__ - 2) );
  VERIFY( stdc_first_leading_zero_ui(UINT_MAX ^ 0b10111) == (__INT_WIDTH__ - 4) );
  VERIFY( stdc_first_leading_zero_ul(ULONG_MAX ^ 0b10111) == (__LONG_WIDTH__ - 4) );
  VERIFY( stdc_first_leading_zero_ull(ULLONG_MAX ^ 0b10111) == (__LONG_LONG_WIDTH__ - 4) );

  VERIFY( stdc_first_leading_zero(0U) == 1 );
  VERIFY( stdc_first_leading_zero(-1U ^ 0b1111) == (__INT_WIDTH__ - 3) );

  static_assert( std::is_same_v<decltype(stdc_first_leading_zero(0UL)), unsigned> );
}

void
test_first_leading_one()
{
  VERIFY( stdc_first_leading_one_uc(0) == 0 );
  VERIFY( stdc_first_leading_one_uc(0b00100) == (CHAR_BIT - 2) );
  VERIFY( stdc_first_leading_one_ui(0b001100) == (__INT_WIDTH__ - 3) );
  VERIFY( stdc_first_leading_one_ul(0b101100) == (__LONG_WIDTH__ - 5) );
  VERIFY( stdc_first_leading_one_ull(0b1110000) == (__LONG_LONG_WIDTH__ - 6) );

  VERIFY( stdc_first_leading_one(0U) == 0 );
  VERIFY( stdc_first_leading_one(-1U >> 4) == 5 );
  VERIFY( stdc_first_leading_one(-1ULL >> 43) == 44 );

  static_assert( std::is_same_v<decltype(stdc_first_leading_one(0UL)), unsigned> );
}

void
test_first_trailing_zero()
{
  VERIFY( stdc_first_trailing_zero_uc(0) == 1 );
  VERIFY( stdc_first_trailing_zero_uc(1) == 2 );
  VERIFY( stdc_first_trailing_zero_uc(7) == 4 );
  VERIFY( stdc_first_trailing_zero_us(15) == 5 );
  VERIFY( stdc_first_trailing_zero_ui(15) == 5 );
  VERIFY( stdc_first_trailing_zero_ul(15) == 5 );
  VERIFY( stdc_first_trailing_zero_ull(15) == 5 );

  VERIFY( stdc_first_trailing_zero(15U) == 5 );

  static_assert( std::is_same_v<decltype(stdc_first_trailing_zero(0UL)), unsigned> );
}

void
test_first_trailing_one()
{
  VERIFY( stdc_first_trailing_one_uc(0) == 0 );
  VERIFY( stdc_first_trailing_one_uc(1) == 1 );
  VERIFY( stdc_first_trailing_one_uc(7) == 1 );
  VERIFY( stdc_first_trailing_one_us(16) == 5 );
  VERIFY( stdc_first_trailing_one_ui(16) == 5 );
  VERIFY( stdc_first_trailing_one_ul(16) == 5 );
  VERIFY( stdc_first_trailing_one_ull(16) == 5 );

  VERIFY( stdc_first_trailing_one(16U) == 5 );
  VERIFY( stdc_first_trailing_one(-1ULL << 17) == 18 );

  static_assert( std::is_same_v<decltype(stdc_first_trailing_one(0UL)), unsigned> );
}

void
test_count_zeros()
{
  VERIFY( stdc_count_zeros_uc(0b101010) == (CHAR_BIT - 3) );
  VERIFY( stdc_count_zeros_us(0b1010101) == (__SHRT_WIDTH__ - 4) );
  VERIFY( stdc_count_zeros_ui(0b1010101111) == (__INT_WIDTH__ - 7) );
  VERIFY( stdc_count_zeros_ul(0b10101011110101) == (__LONG_WIDTH__ - 9) );
  VERIFY( stdc_count_zeros_ull(0b10101011110101) == (__LONG_LONG_WIDTH__ - 9) );

  VERIFY( stdc_count_zeros(0b111UL) == (__LONG_WIDTH__ - 3) );
  VERIFY( stdc_count_zeros(0U) == __INT_WIDTH__ );

  // std::popcount returns signed int, stdc_count_zeros_uc returns unsigned int.
  static_assert( std::is_same_v<decltype(stdc_count_zeros_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_zeros_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_zeros_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_zeros_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_zeros_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_zeros_uc(0u)), unsigned> );
}

void
test_count_ones()
{
  VERIFY( stdc_count_ones_uc(0b101010) == 3 );
  VERIFY( stdc_count_ones_us(0b1010101) == 4 );
  VERIFY( stdc_count_ones_ui(0b1010101111) == 7 );
  VERIFY( stdc_count_ones_ul(0b10101011110101) == 9 );
  VERIFY( stdc_count_ones_ull(0b10101011110101) == 9 );

  VERIFY( stdc_count_ones(0b10101011110101U) == 9 );
  VERIFY( stdc_count_ones(0U) == 0 );

  // std::popcount returns signed int, stdc_count_ones_uc returns unsigned int.
  static_assert( std::is_same_v<decltype(stdc_count_ones_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_ones_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_ones_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_ones_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_ones_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_count_ones_uc(0u)), unsigned> );
}

void
test_has_single_bit()
{
  VERIFY ( ! stdc_has_single_bit_uc(0) );
  VERIFY ( ! stdc_has_single_bit_uc(41) );
  VERIFY ( stdc_has_single_bit_uc(1) );
  VERIFY ( stdc_has_single_bit_uc(64) );
  VERIFY ( stdc_has_single_bit_us(1 << 7) );
  VERIFY ( stdc_has_single_bit_ui(1 << 11) );
  VERIFY ( stdc_has_single_bit_ul(1 << 14) );
  VERIFY ( stdc_has_single_bit_ull(1 << 24) );

  VERIFY ( stdc_has_single_bit(64U) );
  VERIFY ( stdc_has_single_bit(128UL) );
  VERIFY ( ! stdc_has_single_bit(129UL) );
}

void
test_bit_width()
{
  VERIFY( stdc_bit_width_uc(0) == 0 );
  VERIFY( stdc_bit_width_uc(7) == 3 );
  VERIFY( stdc_bit_width_uc(0b10101) == 5 );
  VERIFY( stdc_bit_width_us(0b101010) == 6 );
  VERIFY( stdc_bit_width_ui(0b1010101) == 7 );
  VERIFY( stdc_bit_width_ul(ULONG_MAX) == __LONG_WIDTH__ );
  VERIFY( stdc_bit_width_ull(ULLONG_MAX >> 2) == (__LONG_LONG_WIDTH__ - 2) );

  VERIFY( stdc_bit_width(0b1010101U) == 7U );

  // std::bit_width returns signed int, stdc_bit_width returns unsigned int.
  static_assert( std::is_same_v<decltype(stdc_bit_width_uc(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_bit_width_us(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_bit_width_ui(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_bit_width_ul(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_bit_width_ul(0)), unsigned> );
  static_assert( std::is_same_v<decltype(stdc_bit_width(0u)), unsigned> );
}

void
test_bit_floor()
{
  VERIFY( stdc_bit_floor_uc(0) == 0 );
  VERIFY( stdc_bit_floor_uc(7) == 4 );
  VERIFY( stdc_bit_floor_uc(0b10101) == 0b10000 );
  VERIFY( stdc_bit_floor_us(0b101010) == 0b100000 );
  VERIFY( stdc_bit_floor_ui(0b1010101) == 0b1000000 );
  VERIFY( stdc_bit_floor_ul(ULONG_MAX) == (1ul << (__LONG_WIDTH__ - 1)) );
  VERIFY( stdc_bit_floor_ull(1000000) == (1ULL << 19) );

  VERIFY( stdc_bit_floor(0b1010101U) == 0b1000000 );
}

void
test_bit_ceil()
{
  VERIFY( stdc_bit_ceil_uc(0) == 1 );
  VERIFY( stdc_bit_ceil_uc(1) == 1 );
  VERIFY( stdc_bit_ceil_uc(2) == 2 );
  VERIFY( stdc_bit_ceil_uc(3) == 4 );
  VERIFY( stdc_bit_ceil_us(11) == 16 );
  VERIFY( stdc_bit_ceil_ui(257) == 512 );
  VERIFY( stdc_bit_ceil_ul(1048) == 2048 );
  VERIFY( stdc_bit_ceil_ull(1000000) == (1ULL << 20) );

  VERIFY( stdc_bit_ceil(0b1010101U) == 0b10000000 );
}

int main()
{
  test_leading_zeros();
  test_leading_ones();
  test_trailing_zeros();
  test_trailing_ones();
  test_first_leading_zero();
  test_first_leading_one();
  test_first_trailing_zero();
  test_first_trailing_one();
  test_count_zeros();
  test_count_ones();
  test_has_single_bit();
  test_bit_width();
  test_bit_floor();
  test_bit_ceil();
}

#include <bit>
// The standard doesn't require these values to match,
// so this is specific to libstdc++.
#ifdef _GLIBCXX_RELEASE
static_assert(__STDC_ENDIAN_BIG__    == (int)std::endian::big);
static_assert(__STDC_ENDIAN_LITTLE__ == (int)std::endian::little);
static_assert(__STDC_ENDIAN_NATIVE__ == (int)std::endian::native);
#endif

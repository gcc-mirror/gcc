// Copyright (C) 2020-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// only: float|double|ldouble * * *
// expensive: * [1-9] * *
#include "bits/main.h"

template <typename V>
  void
  test()
  {
    using int_v = std::experimental::fixed_size_simd<int, V::size()>;
    using T = typename V::value_type;
#if __GCC_IEC_559 >= 2 || defined __STDC_IEC_559__
    constexpr auto denorm_min = std::__denorm_min_v<T>;
#endif
#if __GCC_IEC_559 >= 2
    constexpr auto norm_min = std::__norm_min_v<T>;
#endif
    constexpr auto max = std::__finite_max_v<T>;
#if defined __STDC_IEC_559__
    constexpr auto nan = std::__quiet_NaN_v<T>;
    constexpr auto inf = std::__infinity_v<T>;
#endif
    test_values<V>(
      {0, 0.25, 0.5, 1, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
       20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 32, 31, -0., -0.25, -0.5, -1,
       -3, -4, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18,
       -19, -20, -21, -22, -23, -24, -25, -26, -27, -28, -29, -32, -31,
#if __GCC_IEC_559 >= 2
       denorm_min, -denorm_min, norm_min / 2, -norm_min / 2,
#endif
       max, -max, max * 0.123f, -max * 0.123f},
      [](const V input) {
	V expectedFraction;
	const int_v expectedExponent([&](auto i) {
	  int exp;
	  expectedFraction[i] = std::frexp(input[i], &exp);
	  return exp;
	});
	int_v exponent = {};
	const V fraction = frexp(input, &exponent);
	COMPARE(fraction, expectedFraction) << ", input = " << input
	  << ", delta: " << fraction - expectedFraction;
	COMPARE(exponent, expectedExponent)
	  << "\ninput: " << input << ", fraction: " << fraction;
      });
#ifdef __STDC_IEC_559__
    test_values<V>(
      // If x is a NaN, a NaN is returned, and the value of *exp is unspecified.
      //
      // If x is positive  infinity  (negative  infinity),  positive  infinity
      // (negative infinity) is returned, and the value of *exp is unspecified.
      // This behavior is only guaranteed with C's Annex F when __STDC_IEC_559__
      // is defined.
      {nan, inf, -inf, denorm_min, denorm_min * 1.72, -denorm_min,
       -denorm_min * 1.72, 0., -0., 1, -1},
      [](const V input) {
	const V expectedFraction([&](auto i) {
	  int exp;
	  return std::frexp(input[i], &exp);
	});
	int_v exponent = {};
	const V fraction = frexp(input, &exponent);
	COMPARE(isnan(fraction), isnan(expectedFraction))
	  << fraction << ", input = " << input
	  << ", delta: " << fraction - expectedFraction;
	COMPARE(isinf(fraction), isinf(expectedFraction))
	  << fraction << ", input = " << input
	  << ", delta: " << fraction - expectedFraction;
	COMPARE(signbit(fraction), signbit(expectedFraction))
	  << fraction << ", input = " << input
	  << ", delta: " << fraction - expectedFraction;
      });
#endif
  }

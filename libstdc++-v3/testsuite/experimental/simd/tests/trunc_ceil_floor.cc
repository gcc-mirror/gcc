// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
    using T = typename V::value_type;
#ifdef __STDC_IEC_559__
    constexpr T inf = std::__infinity_v<T>;
    constexpr T denorm_min = std::__denorm_min_v<T>;
#endif
    constexpr T norm_min = std::__norm_min_v<T>;
    constexpr T max = std::__finite_max_v<T>;
    constexpr T min = std::__finite_min_v<T>;
    test_values<V>(
      {2.1,
       2.0,
       2.9,
       2.5,
       2.499,
       1.5,
       1.499,
       1.99,
       0.99,
       0.5,
       0.499,
       0.,
       -2.1,
       -2.0,
       -2.9,
       -2.5,
       -2.499,
       -1.5,
       -1.499,
       -1.99,
       -0.99,
       -0.5,
       -0.499,
       3 << 21,
       3 << 22,
       3 << 23,
       -(3 << 21),
       -(3 << 22),
       -(3 << 23),
#ifdef __STDC_IEC_559__
       -0.,
       inf,
       -inf,
       denorm_min,
       norm_min * 0.9,
       -denorm_min,
       -norm_min * 0.9,
#endif
       max,
       norm_min,
       min,
       -norm_min
      },
      [](const V input) {
	const V expected([&](auto i) { return std::trunc(input[i]); });
	COMPARE(trunc(input), expected) << input;
      },
      [](const V input) {
	const V expected([&](auto i) { return std::ceil(input[i]); });
	COMPARE(ceil(input), expected) << input;
      },
      [](const V input) {
	const V expected([&](auto i) { return std::floor(input[i]); });
	COMPARE(floor(input), expected) << input;
      });

#ifdef __STDC_IEC_559__
    test_values<V>(
      {
#ifdef __SUPPORT_SNAN__
	std::__signaling_NaN_v<T>,
#endif
	std::__quiet_NaN_v<T>},
      [](const V input) {
	const V expected([&](auto i) { return std::trunc(input[i]); });
	COMPARE(isnan(trunc(input)), isnan(expected)) << input;
      },
      [](const V input) {
	const V expected([&](auto i) { return std::ceil(input[i]); });
	COMPARE(isnan(ceil(input)), isnan(expected)) << input;
      },
      [](const V input) {
	const V expected([&](auto i) { return std::floor(input[i]); });
	COMPARE(isnan(floor(input)), isnan(expected)) << input;
      });
#endif
  }

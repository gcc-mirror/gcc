// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
#include <cfenv>

template <typename F>
  auto
  verify_no_fp_exceptions(F&& fun)
  {
    std::feclearexcept(FE_ALL_EXCEPT);
    auto r = fun();
    COMPARE(std::fetestexcept(FE_ALL_EXCEPT), 0);
    return r;
  }

#define NOFPEXCEPT(...) verify_no_fp_exceptions([&]() { return __VA_ARGS__; })

template <typename V>
  void
  test()
  {
    using T = typename V::value_type;
    using intv = std::experimental::fixed_size_simd<int, V::size()>;
#if __GCC_IEC_559 >= 2
    constexpr T inf = std::__infinity_v<T>;
    constexpr T denorm_min = std::__infinity_v<T>;
    constexpr T nan = std::__quiet_NaN_v<T>;
#endif
    constexpr T max = std::__finite_max_v<T>;
    constexpr T norm_min = std::__norm_min_v<T>;
    test_values<V>(
      {0., 1., -1.,
#if __GCC_IEC_559 >= 2
       -0., inf, -inf, denorm_min, -denorm_min, nan,
       norm_min * 0.9, -norm_min * 0.9,
#endif
       max, -max, norm_min, -norm_min
      },
      [](const V input) {
	COMPARE(NOFPEXCEPT(isfinite(input)),
		!V([&](auto i) { return std::isfinite(input[i]) ? 0 : 1; }))
	  << input;
	COMPARE(NOFPEXCEPT(isinf(input)),
		!V([&](auto i) { return std::isinf(input[i]) ? 0 : 1; }))
	  << input;
	COMPARE(NOFPEXCEPT(isnan(input)),
		!V([&](auto i) { return std::isnan(input[i]) ? 0 : 1; }))
	  << input;
	COMPARE(NOFPEXCEPT(isnormal(input)),
		!V([&](auto i) { return std::isnormal(input[i]) ? 0 : 1; }))
	  << input;
	COMPARE(NOFPEXCEPT(signbit(input)),
		!V([&](auto i) { return std::signbit(input[i]) ? 0 : 1; }))
	  << input;
	COMPARE(NOFPEXCEPT(isunordered(input, V())),
		!V([&](auto i) { return std::isunordered(input[i], 0) ? 0 : 1; }))
	  << input;
	COMPARE(NOFPEXCEPT(isunordered(V(), input)),
		!V([&](auto i) { return std::isunordered(0, input[i]) ? 0 : 1; }))
	  << input;
	COMPARE(NOFPEXCEPT(fpclassify(input)),
		intv([&](auto i) { return std::fpclassify(input[i]); }))
	  << input;
      });
#ifdef __SUPPORT_SNAN__
    const V snan = std::__signaling_NaN_v<T>;
    COMPARE(isfinite(snan),
	    !V([&](auto i) { return std::isfinite(snan[i]) ? 0 : 1; }))
      << snan;
    COMPARE(isinf(snan), !V([&](auto i) { return std::isinf(snan[i]) ? 0 : 1; }))
      << snan;
    COMPARE(isnan(snan), !V([&](auto i) { return std::isnan(snan[i]) ? 0 : 1; }))
      << snan;
    COMPARE(isnormal(snan),
	    !V([&](auto i) { return std::isnormal(snan[i]) ? 0 : 1; }))
      << snan;
    COMPARE(signbit(snan),
	    !V([&](auto i) { return std::signbit(snan[i]) ? 0 : 1; }))
      << snan;
    COMPARE(isunordered(snan, V()),
	    !V([&](auto i) { return std::isunordered(snan[i], 0) ? 0 : 1; }))
      << snan;
    COMPARE(isunordered(V(), snan),
	    !V([&](auto i) { return std::isunordered(0, snan[i]) ? 0 : 1; }))
      << snan;
    COMPARE(fpclassify(snan),
	    intv([&](auto i) { return std::fpclassify(snan[i]); }))
      << snan;
#endif
  }

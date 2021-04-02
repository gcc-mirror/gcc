// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
// skip: ldouble * powerpc64* *
// expensive: * [1-9] * *
#include "bits/verify.h"
#include "bits/metahelpers.h"
#include "bits/test_values.h"

// 3-arg std::hypot needs to be fixed, this is a better reference:
template <typename T>
  [[gnu::optimize("-fno-unsafe-math-optimizations")]]
  T
  hypot3(T x, T y, T z)
  {
    x = std::abs(x);
    y = std::abs(y);
    z = std::abs(z);
    if (std::isinf(x) || std::isinf(y) || std::isinf(z))
      return std::__infinity_v<T>;
    else if (std::isnan(x) || std::isnan(y) || std::isnan(z))
      return std::__quiet_NaN_v<T>;
    else if (x == y && y == z)
      return x * std::sqrt(T(3));
    else if (z == 0 && y == 0)
      return x;
    else if (x == 0 && z == 0)
      return y;
    else if (x == 0 && y == 0)
      return z;
    else
      {
	T hi = std::max(std::max(x, y), z);
	T lo0 = std::min(std::max(x, y), z);
	T lo1 = std::min(x, y);
	int e = 0;
	hi = std::frexp(hi, &e);
	lo0 = std::ldexp(lo0, -e);
	lo1 = std::ldexp(lo1, -e);
	T lo = lo0 * lo0 + lo1 * lo1;
	return std::ldexp(std::sqrt(hi * hi + lo), e);
      }
  }

template <typename V>
  void
  test()
  {
    vir::test::setFuzzyness<float>(1);
    vir::test::setFuzzyness<double>(1);
    vir::test::setFuzzyness<long double>(2); // because of the bad reference

    using T = typename V::value_type;
    test_values_3arg<V>(
      {
#ifdef __STDC_IEC_559__
	std::__quiet_NaN_v<T>,
	std::__infinity_v<T>,
	-std::__infinity_v<T>,
	std::__norm_min_v<T> / 3,
	-0.,
	std::__denorm_min_v<T>,
#endif
	0.,
	1.,
	-1.,
	std::__norm_min_v<T>,
	-std::__norm_min_v<T>,
	2.,
	-2.,
	std::__finite_max_v<T> / 5,
	std::__finite_max_v<T> / 3,
	std::__finite_max_v<T> / 2,
	-std::__finite_max_v<T> / 5,
	-std::__finite_max_v<T> / 3,
	-std::__finite_max_v<T> / 2,
#ifdef __FAST_MATH__
	// fast-math hypot is imprecise for the max exponent
      },
      {100000, std::__finite_max_v<T> / 2},
#else
	std::__finite_max_v<T>, -std::__finite_max_v<T>},
      {100000},
#endif
      MAKE_TESTER_2(hypot, hypot3));
#if !__FINITE_MATH_ONLY__
    COMPARE(hypot(V(std::__finite_max_v<T>), V(std::__finite_max_v<T>), V()),
	    V(std::__infinity_v<T>));
    COMPARE(hypot(V(std::__finite_max_v<T>), V(), V(std::__finite_max_v<T>)),
	    V(std::__infinity_v<T>));
    COMPARE(hypot(V(), V(std::__finite_max_v<T>), V(std::__finite_max_v<T>)),
	    V(std::__infinity_v<T>));
#endif
    COMPARE(hypot(V(std::__norm_min_v<T>), V(std::__norm_min_v<T>),
		  V(std::__norm_min_v<T>)),
	    V(std::__norm_min_v<T> * std::sqrt(T(3))));
    auto&& hypot3_test
      = [](auto a, auto b, auto c) -> decltype(hypot(a, b, c)) { return {}; };
    VERIFY((sfinae_is_callable<V, V, V>(hypot3_test)));
    VERIFY((sfinae_is_callable<T, T, V>(hypot3_test)));
    VERIFY((sfinae_is_callable<V, T, T>(hypot3_test)));
    VERIFY((sfinae_is_callable<T, V, T>(hypot3_test)));
    VERIFY((sfinae_is_callable<T, V, V>(hypot3_test)));
    VERIFY((sfinae_is_callable<V, T, V>(hypot3_test)));
    VERIFY((sfinae_is_callable<V, V, T>(hypot3_test)));
    VERIFY((sfinae_is_callable<int, int, V>(hypot3_test)));
    VERIFY((sfinae_is_callable<int, V, int>(hypot3_test)));
    VERIFY((sfinae_is_callable<V, T, int>(hypot3_test)));
    VERIFY(!(sfinae_is_callable<bool, V, V>(hypot3_test)));
    VERIFY(!(sfinae_is_callable<V, bool, V>(hypot3_test)));
    VERIFY(!(sfinae_is_callable<V, V, bool>(hypot3_test)));

    vir::test::setFuzzyness<float>(0);
    vir::test::setFuzzyness<double>(0);
    test_values_3arg<V>(
      {
#ifdef __STDC_IEC_559__
	std::__quiet_NaN_v<T>, std::__infinity_v<T>, -std::__infinity_v<T>, -0.,
	std::__norm_min_v<T> / 3, std::__denorm_min_v<T>,
#endif
	0., std::__norm_min_v<T>, std::__finite_max_v<T>},
      {10000, -std::__finite_max_v<T> / 2, std::__finite_max_v<T> / 2},
      MAKE_TESTER(fma));
    auto&& fma_test
      = [](auto a, auto b, auto c) -> decltype(fma(a, b, c)) { return {}; };
    VERIFY((sfinae_is_callable<V, V, V>(fma_test)));
    VERIFY((sfinae_is_callable<T, T, V>(fma_test)));
    VERIFY((sfinae_is_callable<V, T, T>(fma_test)));
    VERIFY((sfinae_is_callable<T, V, T>(fma_test)));
    VERIFY((sfinae_is_callable<T, V, V>(fma_test)));
    VERIFY((sfinae_is_callable<V, T, V>(fma_test)));
    VERIFY((sfinae_is_callable<V, V, T>(fma_test)));
    VERIFY((sfinae_is_callable<int, int, V>(fma_test)));
    VERIFY((sfinae_is_callable<int, V, int>(fma_test)));
    VERIFY((sfinae_is_callable<V, T, int>(fma_test)));
    VERIFY((!sfinae_is_callable<V, T, bool>(fma_test)));
    VERIFY((!sfinae_is_callable<bool, V, V>(fma_test)));
  }

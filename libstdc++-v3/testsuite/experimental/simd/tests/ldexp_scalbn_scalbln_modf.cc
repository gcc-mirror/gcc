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
    vir::test::setFuzzyness<float>(0);
    vir::test::setFuzzyness<double>(0);

    using T = typename V::value_type;

    // See https://sourceware.org/bugzilla/show_bug.cgi?id=18031
    const bool modf_is_broken = [] {
      volatile T x = T(5e20) / 7;
      T tmp;
      return std::fabs(std::modf(x, &tmp)) >= 1;
    }();
    if (modf_is_broken)
      __builtin_fprintf(stderr,
			"NOTE: Skipping modf because std::modf is broken.\n");

    test_values<V>(
      {
#ifdef __STDC_IEC_559__
	std::__quiet_NaN_v<T>,
	std::__infinity_v<T>,
	-std::__infinity_v<T>,
	-0.,
	std::__denorm_min_v<T>,
	std::__norm_min_v<T> / 3,
	-std::__denorm_min_v<T>,
	-std::__norm_min_v<T> / 3,
#endif
	+0.,
	+1.3,
	-1.3,
	2.1,
	-2.1,
	0.99,
	0.9,
	-0.9,
	-0.99,
	std::__norm_min_v<T>,
	std::__finite_max_v<T>,
	-std::__norm_min_v<T>,
	-std::__finite_max_v<T>},
      {10000},
      [](const V input) {
	for (int exp : {-10000, -100, -10, -1, 0, 1, 10, 100, 10000})
	  {
	    const auto totest = ldexp(input, exp);
	    using R = std::remove_const_t<decltype(totest)>;
	    auto&& expected = [&](const auto& v) -> const R {
	      R tmp = {};
	      using std::ldexp;
	      for (std::size_t i = 0; i < R::size(); ++i)
		{
		  tmp[i] = ldexp(v[i], exp);
		}
	      return tmp;
	    };
	    const R expect1 = expected(input);
	    COMPARE(isnan(totest), isnan(expect1))
	      << "ldexp(" << input << ", " << exp << ") = " << totest
	      << " != " << expect1;
	    FUZZY_COMPARE(ldexp(iif(isnan(expect1), 0, input), exp),
			  expected(iif(isnan(expect1), 0, input)))
	      << "\nclean = " << iif(isnan(expect1), 0, input);
	  }
      },
      [](const V input) {
	for (int exp : {-10000, -100, -10, -1, 0, 1, 10, 100, 10000})
	  {
	    const auto totest = scalbn(input, exp);
	    using R = std::remove_const_t<decltype(totest)>;
	    auto&& expected = [&](const auto& v) -> const R {
	      R tmp = {};
	      using std::scalbn;
	      for (std::size_t i = 0; i < R::size(); ++i)
		{
		  tmp[i] = scalbn(v[i], exp);
		}
	      return tmp;
	    };
	    const R expect1 = expected(input);
	    COMPARE(isnan(totest), isnan(expect1))
	      << "scalbn(" << input << ", " << exp << ") = " << totest
	      << " != " << expect1;
	    FUZZY_COMPARE(scalbn(iif(isnan(expect1), 0, input), exp),
			  expected(iif(isnan(expect1), 0, input)))
	      << "\nclean = " << iif(isnan(expect1), 0, input);
	  }
      },
      [](const V input) {
	for (long exp : {-10000, -100, -10, -1, 0, 1, 10, 100, 10000})
	  {
	    const auto totest = scalbln(input, exp);
	    using R = std::remove_const_t<decltype(totest)>;
	    auto&& expected = [&](const auto& v) -> const R {
	      R tmp = {};
	      using std::scalbln;
	      for (std::size_t i = 0; i < R::size(); ++i)
		{
		  tmp[i] = scalbln(v[i], exp);
		}
	      return tmp;
	    };
	    const R expect1 = expected(input);
	    COMPARE(isnan(totest), isnan(expect1))
	      << "scalbln(" << input << ", " << exp << ") = " << totest
	      << " != " << expect1;
	    FUZZY_COMPARE(scalbln(iif(isnan(expect1), 0, input), exp),
			  expected(iif(isnan(expect1), 0, input)))
	      << "\nclean = " << iif(isnan(expect1), 0, input);
	  }
      },
      [modf_is_broken](const V input) {
	if (modf_is_broken)
	  return;
	V integral = {};
	auto&& expected = [&](const auto& v) -> std::pair<const V, const V> {
	  std::pair<V, V> tmp = {};
	  using std::modf;
	  for (std::size_t i = 0; i < V::size(); ++i)
	    {
	      typename V::value_type tmp2;
	      tmp.first[i] = modf(v[i], &tmp2);
	      tmp.second[i] = tmp2;
	    }
	  return tmp;
	};
#ifdef __STDC_IEC_559__
	const V totest = modf(input, &integral);
	const auto expect1 = expected(input);
	COMPARE(isnan(totest), isnan(expect1.first))
	  << "modf(" << input << ", iptr) = " << totest << " != " << expect1;
	COMPARE(isnan(integral), isnan(expect1.second))
	  << "modf(" << input << ", iptr) = " << totest << " != " << expect1;
	COMPARE(isnan(totest), isnan(integral))
	  << "modf(" << input << ", iptr) = " << totest << " != " << expect1;
	const V clean = iif(isnan(totest), V(), input);
#else
	const V clean = iif(isnormal(input), input, V());
#endif
	const auto expect2 = expected(clean);
	COMPARE(modf(clean, &integral), expect2.first) << "\nclean = " << clean;
	COMPARE(integral, expect2.second);
      });
  }

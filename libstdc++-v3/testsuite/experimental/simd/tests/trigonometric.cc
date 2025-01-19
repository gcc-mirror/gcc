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
    vir::test::setFuzzyness<float>(1);
    vir::test::setFuzzyness<double>(1);

    using T = typename V::value_type;
    test_values<V>(
      {
#ifdef __STDC_IEC_559__
	std::__quiet_NaN_v<T>, std::__infinity_v<T>, -std::__infinity_v<T>, -0.,
	std::__denorm_min_v<T>, std::__norm_min_v<T> / 3,
#endif
	+0., std::__norm_min_v<T>, std::__finite_max_v<T>},
      {10000}, MAKE_TESTER(acos), MAKE_TESTER(tan), MAKE_TESTER(acosh),
      MAKE_TESTER(asinh), MAKE_TESTER(atanh), MAKE_TESTER(cosh),
      MAKE_TESTER(sinh), MAKE_TESTER(tanh));
  }

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
// xfail: run * * * *
// expensive: * [1-9] * *
#include "bits/verify.h"
#include "bits/metahelpers.h"
#include "bits/mathreference.h"
#include "bits/simd_view.h"
#include "bits/test_values.h"

template <typename V>
  void
  test()
  {
    using std::cos;
    using std::sin;
    using T = typename V::value_type;

    vir::test::setFuzzyness<float>(2);
    vir::test::setFuzzyness<double>(1);

    const auto& testdata = referenceData<function::sincos, T>();
    std::experimental::experimental::simd_view<V>(testdata).for_each(
      [&](const V input, const V expected_sin, const V expected_cos) {
	FUZZY_COMPARE(sin(input), expected_sin) << " input = " << input;
	FUZZY_COMPARE(sin(-input), -expected_sin) << " input = " << input;
	FUZZY_COMPARE(cos(input), expected_cos) << " input = " << input;
	FUZZY_COMPARE(cos(-input), expected_cos) << " input = " << input;
      });
  }

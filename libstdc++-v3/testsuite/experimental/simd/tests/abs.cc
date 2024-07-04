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

// expensive: * [1-9] * *
#include "bits/main.h"
#include <cmath>    // abs & sqrt
#include <cstdlib>  // integer abs

template <typename V>
  void
  test()
  {
    if constexpr (std::is_signed_v<typename V::value_type>)
      {
	using std::abs;
	using T = typename V::value_type;
	test_values<V>({std::__finite_max_v<T>, std::__norm_min_v<T>,
			-std::__norm_min_v<T>, std::__finite_min_v<T>,
			std::__finite_min_v<T> / 2, T(), -T(), T(-1), T(-2)},
		       {1000}, [](V input) {
			 const V expected(
			   [&](auto i) { return T(std::abs(T(input[i]))); });
			 COMPARE(abs(input), expected) << "input: " << input;
		       });
      }
  }

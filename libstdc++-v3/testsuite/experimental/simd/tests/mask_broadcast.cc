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

// expensive: * [1-9] * *
#include "bits/main.h"

template <typename V>
  void
  test()
  {
    using M = typename V::mask_type;
    static_assert(std::is_convertible<typename M::reference, bool>::value,
		  "A smart_reference<simd_mask> must be convertible to bool.");
    static_assert(
      std::is_same<bool, decltype(std::declval<const typename M::reference&>()
				  == true)>::value,
      "A smart_reference<simd_mask> must be comparable against bool.");
    static_assert(
      vir::test::sfinae_is_callable<typename M::reference&&, bool>(
	[](auto&& a, auto&& b) -> decltype(std::declval<decltype(a)>()
					   == std::declval<decltype(b)>()) {
	  return {};
	}),
      "A smart_reference<simd_mask> must be comparable against bool.");
    VERIFY(std::experimental::is_simd_mask_v<M>);

    {
      M x;     // uninitialized
      x = M{}; // default broadcasts 0
      COMPARE(x, M(false));
      COMPARE(x, M());
      COMPARE(x, M{});
      x = M(); // default broadcasts 0
      COMPARE(x, M(false));
      COMPARE(x, M());
      COMPARE(x, M{});
      x = x;
      for (std::size_t i = 0; i < M::size(); ++i)
	{
	  COMPARE(x[i], false);
	}
    }

    M x(true);
    M y(false);
    for (std::size_t i = 0; i < M::size(); ++i)
      {
	COMPARE(x[i], true);
	COMPARE(y[i], false);
      }
    y = M(true);
    COMPARE(x, y);
  }

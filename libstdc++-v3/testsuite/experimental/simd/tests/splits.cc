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
    using namespace std::experimental::parallelism_v2;
    using T = typename V::value_type;
    if constexpr (V::size() / simd_size_v<T> * simd_size_v<T> == V::size())
      {
	M k(true);
	VERIFY(all_of(k)) << k;
	const auto parts = split<simd_mask<T>>(k);
	for (auto k2 : parts)
	  {
	    VERIFY(all_of(k2)) << k2;
	    COMPARE(typeid(k2), typeid(simd_mask<T>));
	  }
      }
  }

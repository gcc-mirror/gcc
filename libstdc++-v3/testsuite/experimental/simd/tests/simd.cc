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
    using T = typename V::value_type;

    // V must store V::size() values of type T giving us the lower bound on the
    // sizeof
    VERIFY(sizeof(V) >= sizeof(T) * V::size());

    // For fixed_size, V should not pad more than to the next-power-of-2 of
    // sizeof(T) * V::size() (for ABI stability of V), giving us the upper bound
    // on the sizeof. For non-fixed_size we give the implementation a bit more
    // slack to trade space vs. efficiency.
    auto n = sizeof(T) * V::size();
    if (n & (n - 1))
      {
	n = ((n << 1) & ~n) & ~((n >> 1) | (n >> 3));
	while (n & (n - 1))
	  n &= n - 1;
      }
    if constexpr (
      !std::is_same_v<typename V::abi_type,
		      std::experimental::simd_abi::fixed_size<V::size()>>)
      n *= 2;
    VERIFY(sizeof(V) <= n) << "\nsizeof(V): " << sizeof(V) << "\nn: " << n;
  }

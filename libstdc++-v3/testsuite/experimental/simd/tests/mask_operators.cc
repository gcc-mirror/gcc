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
    { // compares
      M x(true), y(false);
      VERIFY(all_of(x == x));
      VERIFY(all_of(x != y));
      VERIFY(all_of(y != x));
      VERIFY(!all_of(x != x));
      VERIFY(!all_of(x == y));
      VERIFY(!all_of(y == x));
    }
    { // subscripting
      M x(true);
      for (std::size_t i = 0; i < M::size(); ++i)
	{
	  COMPARE(x[i], true) << "\nx: " << x << ", i: " << i;
	  x[i] = !x[i];
	}
      COMPARE(x, M{false});
      for (std::size_t i = 0; i < M::size(); ++i)
	{
	  COMPARE(x[i], false) << "\nx: " << x << ", i: " << i;
	  x[i] = !x[i];
	}
      COMPARE(x, M{true});
    }
    { // negation
      M x(false);
      M y = !x;
      COMPARE(y, M{true});
      COMPARE(!y, x);
    }
  }


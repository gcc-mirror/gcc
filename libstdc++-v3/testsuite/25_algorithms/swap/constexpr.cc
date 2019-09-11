// Copyright (C) 2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <algorithm>

constexpr bool
test()
{
  auto ok = true;

  double asc = 3.1415;
  double bsc = 2.7182;
  std::swap(asc, bsc);
  ok = ok && asc == 2.7182 && bsc == 3.1415;

  float arr[5]{0.0f, 1.0f, 2.0f, 3.0f, 4.0f};
  float brr[5]{5.0f, 6.0f, 7.0f, 8.0f, 9.0f};
  std::swap(arr, brr);
  ok = ok && arr[2] == 7.0f && brr[2] == 2.0f;

  return ok;
}

static_assert(test());

// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <valarray>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  int arr[4] = { 1, 2, 3, 4 };
  std::valarray v(arr, 4);
  check_type<std::valarray<int>>(v);

  std::valarray v2 = v;
  check_type<std::valarray<int>>(v2);

  std::valarray v3 = std::move(v);
  check_type<std::valarray<int>>(v3);
}

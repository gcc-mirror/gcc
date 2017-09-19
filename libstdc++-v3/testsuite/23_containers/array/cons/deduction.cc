// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <array>

template<typename T, typename U> struct require_same;
template<typename T> struct require_same<T, T> { using type = void; };

template<typename T, typename U>
  typename require_same<T, U>::type
  check_type(U&) { }

void
test01()
{
  std::array a1{ 1, 2, 3 };
  check_type<std::array<int, 3>>(a1);
  int y = 2;
  const int z = 3;
  std::array a2{ 1, y, z };
  check_type<std::array<int, 3>>(a2);
  std::array a3{ 'a', 'b', 'c', 'd', 'e' };
  check_type<std::array<char, 5>>(a3);

  std::array copy = a1;
  check_type<decltype(a1)>(copy);
  std::array move = std::move(a1);
  check_type<decltype(a1)>(move);
}

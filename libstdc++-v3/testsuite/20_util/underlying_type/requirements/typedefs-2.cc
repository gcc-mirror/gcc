// { dg-do compile { target c++11 } }
//
// Copyright (C) 2011-2023 Free Software Foundation, Inc.
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

// 
// NB: This file is for testing type_traits with NO OTHER INCLUDES.

#include <type_traits>

void test01()
{
  using namespace std;

  enum E1 : unsigned { };
  enum E2 : char { };
  enum class E3 { };
  enum class E4 : unsigned char { c = 1 };
  enum class E5 : int { a = -1, b = 1 };
  enum class E6 : long { c = __LONG_MAX__ };  

  static_assert(is_same<underlying_type<E1>::type, unsigned>::value, "Error");
  static_assert(is_same<underlying_type<E2>::type, char>::value, "Error");
  static_assert(is_same<underlying_type<E3>::type, int>::value, "Error");
  static_assert(is_same<underlying_type<E4>::type,
			unsigned char>::value, "Error");
  static_assert(is_same<underlying_type<E5>::type, int>::value, "Error");
  static_assert(is_same<underlying_type<E6>::type, long>::value, "Error");
}

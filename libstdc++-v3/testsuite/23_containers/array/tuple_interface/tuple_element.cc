// { dg-do compile { target c++11 } }
//
// Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

// NB: Don't include any other headers in this file.
// LWG 2212 requires <array> to define tuple_element<cv T> specializations.
#include <array>

void
test01() 
{
  using std::array;
  using std::tuple_element;
  // This relies on the fact that <utility> includes <type_traits>:
  using std::is_same;

  const size_t len = 3;
  typedef array<int, len> array_type;

  static_assert(is_same<tuple_element<0, array_type>::type, int>::value, "" );
  static_assert(is_same<tuple_element<1, array_type>::type, int>::value, "" );
  static_assert(is_same<tuple_element<2, array_type>::type, int>::value, "");

  static_assert(is_same<tuple_element<0, const array_type>::type,
                const int>::value, "");
  static_assert(is_same<tuple_element<1, const array_type>::type,
                const int>::value, "");
  static_assert(is_same<tuple_element<2, const array_type>::type,
                const int>::value, "");

  static_assert(is_same<tuple_element<0, volatile array_type>::type,
                volatile int>::value, "");
  static_assert(is_same<tuple_element<1, volatile array_type>::type,
                volatile int>::value, "");
  static_assert( (is_same<tuple_element<2, volatile array_type>::type,
           volatile int>::value == true), "" );

  static_assert(is_same<tuple_element<0, const volatile array_type>::type,
                const volatile int>::value, "");
  static_assert(is_same<tuple_element<1, const volatile array_type>::type,
                const volatile int>::value, "");
  static_assert(is_same<tuple_element<2, const volatile array_type>::type,
                const volatile int>::value, "");
}

int main()
{
  test01();
  return 0;
}

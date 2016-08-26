// { dg-do compile { target c++11 } }
//
// Copyright (C) 2011-2016 Free Software Foundation, Inc.
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
// LWG 2212 requires <array> to define tuple_size<cv T> specializations.
#include <array>

void
test01() 
{ 
  bool test __attribute__((unused)) = true;
  using std::array;
  using std::tuple_size;
  // This relies on the fact that <utility> includes <type_traits>:
  using std::is_same;

  {
    const size_t len = 5;
    typedef array<int, len> array_type;
    static_assert(tuple_size<array_type>::value == 5, "");
    static_assert(tuple_size<const array_type>::value == 5, "");
    static_assert(tuple_size<volatile array_type>::value == 5, "");
    static_assert(tuple_size<const volatile array_type>::value == 5, "");
  }

  {
    const size_t len = 0;
    typedef array<float, len> array_type;
    static_assert(tuple_size<array_type>::value == 0, "");
    static_assert(tuple_size<const array_type>::value == 0, "");
    static_assert(tuple_size<volatile array_type>::value == 0, "");
    static_assert(tuple_size<const volatile array_type>::value == 0, "");
  }
}

int main()
{
  test01();
  return 0;
}

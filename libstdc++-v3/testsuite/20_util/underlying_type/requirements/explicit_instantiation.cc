// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

// NB: This file is for testing type_traits with NO OTHER INCLUDES.

#include <type_traits>

namespace std
{
  typedef enum E1 : unsigned { }                     test_type1;
  typedef enum E2 : char { }                         test_type2;
  typedef enum class E3 { }                          test_type3;
  typedef enum class E4 : unsigned char { c = 1 }    test_type4;
  typedef enum class E5 : int { a = -1, b = 1 }      test_type5;
  typedef enum class E6 : long { c = __LONG_MAX__ }  test_type6;  

  template struct underlying_type<test_type1>;
  template struct underlying_type<test_type2>;
  template struct underlying_type<test_type3>;
  template struct underlying_type<test_type4>;
  template struct underlying_type<test_type5>;
  template struct underlying_type<test_type6>;
}

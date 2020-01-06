// { dg-do compile { target c++11 } }
// { dg-skip-if "result_type removed for C++20" { c++2a } }

// Copyright (C) 2008-2020 Free Software Foundation, Inc.
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

#include <functional>
#include <type_traits>

struct X {};

struct int_result_type { typedef int result_type; };

struct derives_unary : std::unary_function<int, int> {};

struct derives_binary : std::binary_function<int, float, int> {};

struct derives_unary_binary
  : std::unary_function<int, int>,
    std::binary_function<int, float, int>
{
  typedef int result_type;
};

void test01()
{
  using std::reference_wrapper;
  using std::is_same;

  // Check result_type typedef
  static_assert( is_same<reference_wrapper<int_result_type>::result_type, int>::value, "has result_type" );
  static_assert( is_same<reference_wrapper<derives_unary>::result_type, int>::value, "has result_type" );
  static_assert( is_same<reference_wrapper<derives_binary>::result_type, int>::value, "has result_type" );
  static_assert( is_same<reference_wrapper<derives_unary_binary>::result_type, int>::value, "has result_type" );
  static_assert( is_same<reference_wrapper<int(void)>::result_type, int>::value, "has result_type" );
  static_assert( is_same<reference_wrapper<int(*)(void)>::result_type, int>::value, "has result_type" );
  static_assert( is_same<reference_wrapper<int (::X::*)()>::result_type, int>::value, "has result_type" );
  static_assert( is_same<reference_wrapper<int (::X::*)(float)>::result_type, int>::value, "has result_type" );
}

int main()
{
  test01();
  return 0;
}

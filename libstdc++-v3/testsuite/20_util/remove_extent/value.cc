// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <type_traits>
#include <testsuite_tr1.h>

void test01()
{
  using std::remove_extent;
  using std::is_same;
  using namespace __gnu_test;

  static_assert(is_same<remove_extent<int>::type, int>::value, "");
  static_assert(is_same<remove_extent<int[2]>::type, int>::value, "");
  static_assert(is_same<remove_extent<int[2][3]>::type, int[3]>::value, "");
  static_assert(is_same<remove_extent<int[][3]>::type, int[3]>::value, "");
  static_assert(is_same<remove_extent<const int[2]>::type,
		const int>::value, "");
  static_assert(is_same<remove_extent<ClassType>::type, ClassType>::value, "");
  static_assert(is_same<remove_extent<ClassType[2]>::type,
		ClassType>::value, "");
  static_assert(is_same<remove_extent<ClassType[2][3]>::type,
		ClassType[3]>::value, "");
  static_assert(is_same<remove_extent<ClassType[][3]>::type,
		ClassType[3]>::value, "");
  static_assert(is_same<remove_extent<const ClassType[2]>::type,
		const ClassType>::value, "");
}

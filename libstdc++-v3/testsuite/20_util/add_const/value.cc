// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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
  using std::add_const;
  using std::is_same;
  using namespace __gnu_test;

  static_assert(is_same<add_const<int>::type, const int>::value, "");
  static_assert(is_same<add_const<volatile int>::type,
		const volatile int>::value, "");
  static_assert(is_same<add_const<int*>::type, int* const>::value, "");
  static_assert(is_same<add_const<int&>::type, int&>::value, "");
  static_assert(is_same<add_const<int (int)>::type, int (int)>::value, "");
  static_assert(is_same<add_const<const int>::type, const int>::value, "");
  static_assert(is_same<add_const<ClassType>::type,
		const ClassType>::value, "");
  static_assert(is_same<add_const<volatile ClassType>::type,
		const volatile ClassType>::value, "");
  static_assert(is_same<add_const<ClassType*>::type,
		ClassType* const>::value, "");
  static_assert(is_same<add_const<ClassType&>::type, ClassType&>::value, "");
  static_assert(is_same<add_const<ClassType (ClassType)>::type,
		ClassType (ClassType)>::value, "");
  static_assert(is_same<add_const<const ClassType>::type,
		const ClassType>::value, "");
}

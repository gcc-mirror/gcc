// { dg-options "-std=gnu++11" }
// { dg-do compile }

// Copyright (C) 2013 Free Software Foundation, Inc.
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
  using std::add_cv;
  using std::is_same;
  using namespace __gnu_test;

  static_assert(is_same<add_cv<int>::type, const volatile int>::value, "");
  static_assert(is_same<add_cv<const int>::type,
		const volatile int>::value, "");
  static_assert(is_same<add_cv<int*>::type, int* const volatile>::value, "");
  static_assert(is_same<add_cv<int&>::type, int&>::value, "");
  static_assert(is_same<add_cv<int (int)>::type, int (int)>::value, "");
  static_assert(is_same<add_cv<const volatile int>::type,
		const volatile int>::value, "");
  static_assert(is_same<add_cv<ClassType>::type,
		const volatile ClassType>::value, "");
  static_assert(is_same<add_cv<volatile ClassType>::type,
		const volatile ClassType>::value, "");
  static_assert(is_same<add_cv<ClassType*>::type,
		ClassType* const volatile>::value, "");
  static_assert(is_same<add_cv<ClassType&>::type, ClassType&>::value, "");
  static_assert(is_same<add_cv<ClassType (ClassType)>::type,
		ClassType (ClassType)>::value, "");
  static_assert(is_same<add_cv<const volatile ClassType>::type,
		const volatile ClassType>::value, "");
}

// { dg-options "-std=gnu++11" }
// { dg-do compile }

// Copyright (C) 2011-2013 Free Software Foundation, Inc.
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
  using std::is_member_object_pointer;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_category<is_member_object_pointer,
		int (ClassType::*)>(true), "");
  static_assert(test_category<is_member_object_pointer,
		const int (ClassType::*)>(true), "");
  static_assert(test_category<is_member_object_pointer,
		ClassType (ClassType::*)>(true), "");

  // Negative tests.
  static_assert(test_category<is_member_object_pointer,
		int (ClassType::*) (int)>(false), "");
  static_assert(test_category<is_member_object_pointer,
		int (ClassType::*) (int) const>(false), "");
  static_assert(test_category<is_member_object_pointer,
		int (ClassType::*) (float, ...)>(false), "");
  static_assert(test_category<is_member_object_pointer,
		ClassType (ClassType::*) (ClassType)>(false), "");
  static_assert(test_category<is_member_object_pointer,
		float (ClassType::*) (int, float, int[], int&)>(false), "");

  // Sanity check.
  static_assert(test_category<is_member_object_pointer, ClassType>(false), "");
}

int main()
{
  test01();
  return 0;
}

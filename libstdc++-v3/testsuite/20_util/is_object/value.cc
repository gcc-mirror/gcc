// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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
  using std::is_object;
  using namespace __gnu_test;
  
  static_assert(test_category<is_object, int (int)>(false), "");
  static_assert(test_category<is_object, ClassType (ClassType)>(false), "");
  static_assert(test_category<is_object,
		float (int, float, int[], int&)>(false), "");
  static_assert(test_category<is_object, int&>(false), "");
  static_assert(test_category<is_object, ClassType&>(false), "");
  static_assert(test_category<is_object, int(&)(int)>(false), "");
  static_assert(test_category<is_object, void>(false), "");
  static_assert(test_category<is_object, const void>(false), "");

  // Sanity check.
  static_assert(test_category<is_object, ClassType>(true), "");
}

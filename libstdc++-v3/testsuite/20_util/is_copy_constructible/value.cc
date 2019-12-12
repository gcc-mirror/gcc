// { dg-do compile { target c++11 } }
//
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
  using std::is_copy_constructible;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_category<is_copy_constructible, int>(true), "");
  static_assert(test_category<is_copy_constructible, float>(true), "");
  static_assert(test_category<is_copy_constructible, EnumType>(true), "");
  static_assert(test_category<is_copy_constructible, int*>(true), "");
  static_assert(test_category<is_copy_constructible, int(*)(int)>(true), "");
  static_assert(test_category<is_copy_constructible,
		int (ClassType::*)>(true), "");
  static_assert(test_category<is_copy_constructible,
		int (ClassType::*) (int)>(true), "");

  static_assert(test_property<is_copy_constructible,
		NoexceptCopyConsClass>(true), "");
  static_assert(test_property<is_copy_constructible,
		const NoexceptCopyConsClass>(true), "");
  static_assert(test_property<is_copy_constructible,
		ThrowCopyConsClass>(true), "");
  static_assert(test_property<is_copy_constructible,
		ExceptCopyConsClass>(true), "");

  // Negative tests.
  static_assert(test_category<is_copy_constructible, void>(false), "");
  static_assert(test_category<is_copy_constructible, int[2]>(false), "");
  static_assert(test_category<is_copy_constructible, int[]>(false), "");
  static_assert(test_category<is_copy_constructible, float[][3]>(false), "");
  static_assert(test_category<is_copy_constructible,
		EnumType[2][3][4]>(false), "");
  static_assert(test_category<is_copy_constructible, int*[3]>(false), "");
  static_assert(test_category<is_copy_constructible,
		int(*[][2])(int)>(false), "");
  static_assert(test_category<is_copy_constructible,
		int (ClassType::*[2][3])>(false), "");
  static_assert(test_category<is_copy_constructible,
		int (ClassType::*[][2][3]) (int)>(false), "");
  static_assert(test_category<is_copy_constructible,
		ClassType(unsigned) const &>(false), "");
  static_assert(test_category<is_copy_constructible, 
		bool(ClassType) const>(false), "");
  static_assert(test_category<is_copy_constructible, 
		bool(...) &&>(false), "");
  static_assert(test_category<is_copy_constructible, 
		EnumType(int, ...)>(false), "");

  static_assert(test_property<is_copy_constructible,
		volatile NoexceptCopyConsClass>(false), "");
}

// { dg-options "-std=gnu++11" }
// { dg-do compile }

// 2004-12-29  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2004-2013 Free Software Foundation, Inc.
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
  using std::is_nothrow_default_constructible;
  using namespace __gnu_test;

  // Positive tests.  
  static_assert(test_category<is_nothrow_default_constructible, int>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		float>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		EnumType>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int*>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int(*)(int)>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int (ClassType::*)>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int (ClassType::*) (int)>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int[2]>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		EnumType[2][3][4]>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int*[3]>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int (ClassType::*[2][3])>(true), "");
  static_assert(test_category<is_nothrow_default_constructible,
		ClassType>(true), "");

  static_assert(test_category<is_nothrow_default_constructible,
		NoexceptDefaultClass>(true), "");

  // Negative tests.
  static_assert(test_category<is_nothrow_default_constructible,
		void>(false), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int[]>(false), "");
  static_assert(test_category<is_nothrow_default_constructible,
		float[][3]>(false), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int(*[][2])(int)>(false), "");
  static_assert(test_category<is_nothrow_default_constructible,
		int (ClassType::*[][2][3]) (int)>(false), "");

  static_assert(test_category<is_nothrow_default_constructible,
		ThrowDefaultClass>(false), "");
  static_assert(test_category<is_nothrow_default_constructible,
		ExceptDefaultClass>(false), "");
}

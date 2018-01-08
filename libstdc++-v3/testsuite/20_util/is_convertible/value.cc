// { dg-do compile { target c++11 } }

// 2009-10-29  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2009-2018 Free Software Foundation, Inc.
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
  using std::is_convertible;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_relationship<is_convertible, int, int>(true), "");
  static_assert(test_relationship<is_convertible, int, const int>(true), "");
  static_assert(test_relationship<is_convertible,
		volatile int, const int>(true), "");
  static_assert(test_relationship<is_convertible, int, float>(true), "");
  static_assert(test_relationship<is_convertible, double, float>(true), "");
  static_assert(test_relationship<is_convertible, float, int>(true), "");
  static_assert(test_relationship<is_convertible, int*, const int*>(true), "");
  static_assert(test_relationship<is_convertible, int*, void*>(true), "");
  static_assert(test_relationship<is_convertible, int[4], int*>(true), "");
  static_assert(test_relationship<is_convertible, float&, int>(true), "");
  static_assert(test_relationship<is_convertible, int, const int&>(true), ""); 
  static_assert(test_relationship<is_convertible, const int&, int>(true), "");
  static_assert(test_relationship<is_convertible, float, const int&>(true), "");
  static_assert(test_relationship<is_convertible, int(int),
		int(*)(int)>(true), "");
  static_assert(test_relationship<is_convertible,
		int(&)(int), int(*)(int)>(true), "");
  static_assert(test_relationship<is_convertible, EnumType, int>(true), "");
  static_assert(test_relationship<is_convertible, ClassType,
		ClassType>(true), "");
  static_assert(test_relationship<is_convertible, DerivedType,
		ClassType>(true), "");
  static_assert(test_relationship<is_convertible, DerivedType*,
		ClassType*>(true), "");
  static_assert(test_relationship<is_convertible, DerivedType&,
		ClassType&>(true), "");

  static_assert(test_relationship<is_convertible, const int,
		const int&>(true), "");

  static_assert(test_relationship<is_convertible, void, void>(true), "");
  static_assert(test_relationship<is_convertible, const void, void>(true), "");
  static_assert(test_relationship<is_convertible, void,
		volatile void>(true), "");
  static_assert(test_relationship<is_convertible, double&,
		ExplicitClass>(true), "");

  static_assert(test_relationship<is_convertible, int(int),
		int(&)(int)>(true), "");

  // Negative tests.
  static_assert(test_relationship<is_convertible, const int*, int*>(false), "");
  static_assert(test_relationship<is_convertible, int*, float*>(false), "");
  static_assert(test_relationship<is_convertible, const int[4],
		int*>(false), "");
  static_assert(test_relationship<is_convertible, int[4], int[4]>(false), "");
  static_assert(test_relationship<is_convertible, const int&, int&>(false), "");
  static_assert(test_relationship<is_convertible, float&, int&>(false), "");
  static_assert(test_relationship<is_convertible, float,
		volatile int&>(false), "");
  static_assert(test_relationship<is_convertible, int(int),
		int(int)>(false), "");
  static_assert(test_relationship<is_convertible, int(int),
		int(*)(void)>(false), "");
  static_assert(test_relationship<is_convertible, int(*)(int),
		int(&)(int)>(false), "");
  static_assert(test_relationship<is_convertible, int, EnumType>(false), "");
  static_assert(test_relationship<is_convertible, int, ClassType>(false), "");
  static_assert(test_relationship<is_convertible, ClassType,
		DerivedType>(false), "");
  static_assert(test_relationship<is_convertible, ClassType*,
		DerivedType*>(false), "");
  static_assert(test_relationship<is_convertible, ClassType&,
		DerivedType&>(false), "");

  static_assert(test_relationship<is_convertible, void, int>(false), "");
  static_assert(test_relationship<is_convertible, void, float>(false), "");  
  static_assert(test_relationship<is_convertible, void,
		int(*)(int)>(false), "");

  // C++0x
  static_assert(test_relationship<is_convertible, int, void>(false), "");
  static_assert(test_relationship<is_convertible, int[4], void>(false), "");

  static_assert(test_relationship<is_convertible, int, int&>(false), "");
  static_assert(test_relationship<is_convertible, float,
		volatile float&>(false), "");
  static_assert(test_relationship<is_convertible, const volatile int,
		const volatile int&>(false), "");
  static_assert(test_relationship<is_convertible, volatile int,
		volatile int&>(false), "");
  static_assert(test_relationship<is_convertible, int&,
		ExplicitClass>(false), "");
  static_assert(test_relationship<is_convertible, void*,
		ExplicitClass>(false), "");
}

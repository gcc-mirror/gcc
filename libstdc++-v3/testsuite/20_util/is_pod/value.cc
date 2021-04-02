// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wno-deprecated" { target { c++2a } } }

// 2010-02-21  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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
  using std::is_pod;
  using namespace __gnu_test;

  static_assert(test_category<is_pod, int>(true), "");
  static_assert(test_category<is_pod, float>(true), "");
  static_assert(test_category<is_pod, EnumType>(true), "");
  static_assert(test_category<is_pod, int*>(true), "");
  static_assert(test_category<is_pod, int(*)(int)>(true), "");
  static_assert(test_category<is_pod, int (ClassType::*)>(true), "");
  static_assert(test_category<is_pod, int (ClassType::*) (int)>(true), "");
  static_assert(test_category<is_pod, int[2]>(true), "");
  static_assert(test_category<is_pod, float[][3]>(true), "");
  static_assert(test_category<is_pod, EnumType[2][3][4]>(true), "");
  static_assert(test_category<is_pod, int*[3]>(true), "");
  static_assert(test_category<is_pod, int(*[][2])(int)>(true), "");
  static_assert(test_category<is_pod, int (ClassType::*[2][3])>(true), "");
  static_assert(test_category<is_pod,
		int (ClassType::*[][2][3]) (int)>(true), "");
  static_assert(test_category<is_pod, ClassType>(true), "");
  static_assert(test_category<is_pod, PODType>(true), "");

  static_assert(test_category<is_pod, void>(false), "");
  static_assert(test_category<is_pod, NType>(false), "");
  static_assert(test_category<is_pod, TType>(false), "");
  static_assert(test_category<is_pod, SLType>(false), "");
}

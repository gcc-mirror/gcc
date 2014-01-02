// { dg-options "-std=gnu++11" }
// { dg-do compile }

// Copyright (C) 2013-2014 Free Software Foundation, Inc.
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
  using std::extent;
  using namespace __gnu_test;

  static_assert(test_property<extent, int, 0>(0), "");
  static_assert(test_property<extent, int[2], 0>(2), "");
  static_assert(test_property<extent, int[2][4], 0>(2), "");
  static_assert(test_property<extent, int[][4], 0>(0), "");
  static_assert(extent<int, 1>::value == 0, "");
  static_assert(extent<int[2], 1>::value == 0, "");
  static_assert(extent<int[2][4], 1>::value == 4, "");
  static_assert(extent<int[][4], 1>::value == 4, "");
  static_assert(extent<int[10][4][6][8][12][2], 4>::value == 12, "");
  static_assert(test_property<extent, ClassType, 0>(0), "");
  static_assert(test_property<extent, ClassType[2], 0>(2), "");
  static_assert(test_property<extent, ClassType[2][4], 0>(2), "");
  static_assert(test_property<extent, ClassType[][4], 0>(0), "");
  static_assert(extent<ClassType, 1>::value == 0, "");
  static_assert(extent<ClassType[2], 1>::value == 0, "");
  static_assert(extent<ClassType[2][4], 1>::value == 4, "");
  static_assert(extent<ClassType[][4], 1>::value == 4, "");
  static_assert(extent<ClassType[10][4][6][8][12][2], 4>::value == 12, "");
}

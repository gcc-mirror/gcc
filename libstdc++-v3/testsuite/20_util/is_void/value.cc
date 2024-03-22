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
  using std::is_void;
  using namespace __gnu_test;

  static_assert(test_category<is_void, void>(true), "");

  static_assert(test_category<is_void, char>(false), "");
  static_assert(test_category<is_void, signed char>(false), "");
  static_assert(test_category<is_void, unsigned char>(false), "");
  static_assert(test_category<is_void, wchar_t>(false), "");
  static_assert(test_category<is_void, short>(false), "");
  static_assert(test_category<is_void, unsigned short>(false), "");
  static_assert(test_category<is_void, int>(false), "");
  static_assert(test_category<is_void, unsigned int>(false), "");
  static_assert(test_category<is_void, long>(false), "");
  static_assert(test_category<is_void, unsigned long>(false), "");
  static_assert(test_category<is_void, long long>(false), "");
  static_assert(test_category<is_void, unsigned long long>(false), "");
  static_assert(test_category<is_void, float>(false), "");
  static_assert(test_category<is_void, double>(false), "");
  static_assert(test_category<is_void, long double>(false), "");

  // Sanity check.
  static_assert(test_category<is_void, ClassType>(false), "");
  static_assert(test_category<is_void, IncompleteClass>(false), "");
  static_assert(test_category<is_void, IncompleteUnion>(false), "");
}

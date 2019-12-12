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
  using std::is_fundamental;
  using namespace __gnu_test;
  
  static_assert(test_category<is_fundamental, void>(true), "");
  static_assert(test_category<is_fundamental, char>(true), "");
  static_assert(test_category<is_fundamental, signed char>(true), "");
  static_assert(test_category<is_fundamental, unsigned char>(true), "");
#ifdef _GLIBCXX_USE_WCHAR_T
  static_assert(test_category<is_fundamental, wchar_t>(true), "");
#endif
  static_assert(test_category<is_fundamental, short>(true), "");
  static_assert(test_category<is_fundamental, unsigned short>(true), "");
  static_assert(test_category<is_fundamental, int>(true), "");
  static_assert(test_category<is_fundamental, unsigned int>(true), "");
  static_assert(test_category<is_fundamental, long>(true), "");
  static_assert(test_category<is_fundamental, unsigned long>(true), "");
  static_assert(test_category<is_fundamental, long long>(true), "");
  static_assert(test_category<is_fundamental, unsigned long long>(true), "");
  static_assert(test_category<is_fundamental, float>(true), "");
  static_assert(test_category<is_fundamental, double>(true), "");
  static_assert(test_category<is_fundamental, long double>(true), "");

  // libstdc++/56609
  static_assert(test_category<is_fundamental, std::nullptr_t>(true), "");

  // Sanity check.
  static_assert(test_category<is_fundamental, ClassType>(false), "");
}

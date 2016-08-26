// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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
  using std::is_arithmetic;
  using namespace __gnu_test;

  static_assert(test_category<is_arithmetic, void>(false), "");

  static_assert(test_category<is_arithmetic, char>(true), "");
  static_assert(test_category<is_arithmetic, signed char>(true), "");
  static_assert(test_category<is_arithmetic, unsigned char>(true), "");
#ifdef _GLIBCXX_USE_WCHAR_T
  static_assert(test_category<is_arithmetic, wchar_t>(true), "");
#endif
  static_assert(test_category<is_arithmetic, short>(true), "");
  static_assert(test_category<is_arithmetic, unsigned short>(true), "");
  static_assert(test_category<is_arithmetic, int>(true), "");
  static_assert(test_category<is_arithmetic, unsigned int>(true), "");
  static_assert(test_category<is_arithmetic, long>(true), "");
  static_assert(test_category<is_arithmetic, unsigned long>(true), "");
  static_assert(test_category<is_arithmetic, long long>(true), "");
  static_assert(test_category<is_arithmetic, unsigned long long>(true), "");
  static_assert(test_category<is_arithmetic, float>(true), "");
  static_assert(test_category<is_arithmetic, double>(true), "");
  static_assert(test_category<is_arithmetic, long double>(true), "");

  // Sanity check.
  static_assert(test_category<is_arithmetic, ClassType>(false), "");
}

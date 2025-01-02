// { dg-do compile { target c++11 } }

// 2008-05-20  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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
  using std::is_integral;
  using namespace __gnu_test;

  static_assert(test_category<is_integral, void>(false), "");

  static_assert(test_category<is_integral, char>(true), "");
  static_assert(test_category<is_integral, signed char>(true), "");
  static_assert(test_category<is_integral, unsigned char>(true), "");
  static_assert(test_category<is_integral, wchar_t>(true), "");
#ifdef _GLIBCXX_USE_CHAR8_T
  static_assert(test_category<is_integral, char8_t>(true), "");
#endif
  static_assert(test_category<is_integral, char16_t>(true), "");
  static_assert(test_category<is_integral, char32_t>(true), "");
  static_assert(test_category<is_integral, short>(true), "");
  static_assert(test_category<is_integral, unsigned short>(true), "");
  static_assert(test_category<is_integral, int>(true), "");
  static_assert(test_category<is_integral, unsigned int>(true), "");
  static_assert(test_category<is_integral, long>(true), "");
  static_assert(test_category<is_integral, unsigned long>(true), "");
  static_assert(test_category<is_integral, long long>(true), "");
  static_assert(test_category<is_integral, unsigned long long>(true), "");

  static_assert(test_category<is_integral, float>(false), "");
  static_assert(test_category<is_integral, double>(false), "");
  static_assert(test_category<is_integral, long double>(false), "");

#ifndef __STRICT_ANSI__
  // GNU Extensions.
#ifdef __SIZEOF_INT128__
  static_assert(test_category<is_integral, __int128>(true), "");
  static_assert(test_category<is_integral, unsigned __int128>(true), "");
#endif

#ifdef _GLIBCXX_USE_FLOAT128
  static_assert(test_category<is_integral, __float128>(false), "");
#endif
#endif

  // Sanity check.
  static_assert(test_category<is_integral, ClassType>(false), "");
}

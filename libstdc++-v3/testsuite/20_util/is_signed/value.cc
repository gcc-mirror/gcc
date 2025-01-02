// { dg-do compile { target c++11 } }

// 2005-01-24  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
  using std::is_signed;
  using namespace __gnu_test;

  static_assert(test_category<is_signed, void>(false), "");

  static_assert(test_category<is_signed, char>(char(-1) < char(0)), "");
  static_assert(test_category<is_signed, signed char>(true), "");
  static_assert(test_category<is_signed, unsigned char>(false), "");
  static_assert(test_category<is_signed, wchar_t>
		(wchar_t(-1) < wchar_t(0)), "");
  static_assert(test_category<is_signed, short>(true), "");
  static_assert(test_category<is_signed, unsigned short>(false), "");
  static_assert(test_category<is_signed, int>(true), "");
  static_assert(test_category<is_signed, unsigned int>(false), "");
  static_assert(test_category<is_signed, long>(true), "");
  static_assert(test_category<is_signed, unsigned long>(false), "");
  static_assert(test_category<is_signed, long long>(true), "");
  static_assert(test_category<is_signed, unsigned long long>(false), "");

  static_assert(test_category<is_signed, float>(true), "");
  static_assert(test_category<is_signed, double>(true), "");
  static_assert(test_category<is_signed, long double>(true), "");

#ifndef __STRICT_ANSI__
  // GNU Extensions.
#ifdef __SIZEOF_INT128__
  static_assert(test_category<is_signed, __int128>(true), "");
  static_assert(test_category<is_signed, unsigned __int128>(false), "");
#endif

#ifdef _GLIBCXX_USE_FLOAT128
  static_assert(test_category<is_signed, __float128>(true), "");
#endif
#endif

  // Sanity check.
  static_assert(test_category<is_signed, ClassType>(false), "");
}

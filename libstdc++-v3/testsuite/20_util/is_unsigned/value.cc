// { dg-do compile { target c++11 } }

// 2005-01-24  Paolo Carlini  <pcarlini@suse.de>
//
// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
  using std::is_unsigned;
  using namespace __gnu_test;

  static_assert(test_category<is_unsigned, void>(false), "");

  static_assert(test_category<is_unsigned, char>(char(-1) > char(0)), "");
  static_assert(test_category<is_unsigned, signed char>(false), "");
  static_assert(test_category<is_unsigned, unsigned char>(true), "");
  static_assert(test_category<is_unsigned, wchar_t>
		(wchar_t(-1) > wchar_t(0)), "");
  static_assert(test_category<is_unsigned, short>(false), "");
  static_assert(test_category<is_unsigned, unsigned short>(true), "");
  static_assert(test_category<is_unsigned, int>(false), "");
  static_assert(test_category<is_unsigned, unsigned int>(true), "");
  static_assert(test_category<is_unsigned, long>(false), "");
  static_assert(test_category<is_unsigned, unsigned long>(true), "");
  static_assert(test_category<is_unsigned, long long>(false), "");
  static_assert(test_category<is_unsigned, unsigned long long>(true), "");

  static_assert(test_category<is_unsigned, float>(false), "");
  static_assert(test_category<is_unsigned, double>(false), "");
  static_assert(test_category<is_unsigned, long double>(false), "");

#ifndef __STRICT_ANSI__
  // GNU Extensions.
#ifdef __SIZEOF_INT128__
  static_assert(test_category<is_unsigned, unsigned __int128>(true), "");
  static_assert(test_category<is_unsigned, __int128>(false), "");
#endif

#ifdef _GLIBCXX_USE_FLOAT128
  static_assert(test_category<is_unsigned, __float128>(false), "");
#endif
#endif

  // Sanity check.
  static_assert(test_category<is_unsigned, ClassType>(false), "");
}

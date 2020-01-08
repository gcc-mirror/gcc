// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2020 Free Software Foundation, Inc.
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
  using std::is_compound;
  using namespace __gnu_test;

  static_assert(test_category<is_compound, void>(false), "");
  static_assert(test_category<is_compound, char>(false), "");
  static_assert(test_category<is_compound, signed char>(false), "");
  static_assert(test_category<is_compound, unsigned char>(false), "");
#ifdef _GLIBCXX_USE_WCHAR_T
  static_assert(test_category<is_compound, wchar_t>(false), "");
#endif
  static_assert(test_category<is_compound, short>(false), "");
  static_assert(test_category<is_compound, unsigned short>(false), "");
  static_assert(test_category<is_compound, int>(false), "");
  static_assert(test_category<is_compound, unsigned int>(false), "");
  static_assert(test_category<is_compound, long>(false), "");
  static_assert(test_category<is_compound, unsigned long>(false), "");
  static_assert(test_category<is_compound, long long>(false), "");
  static_assert(test_category<is_compound, unsigned long long>(false), "");
  static_assert(test_category<is_compound, float>(false), "");
  static_assert(test_category<is_compound, double>(false), "");
  static_assert(test_category<is_compound, long double>(false), "");

  // libstdc++/56609
  static_assert(test_category<is_compound, std::nullptr_t>(false), "");

  // Sanity check.
  static_assert(test_category<is_compound, ClassType>(true), "");
}

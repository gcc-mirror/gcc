// { dg-do compile { target c++11 } }

// 2010-03-23  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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
  using std::is_literal_type;
  using namespace __gnu_test;

  static_assert(test_category<is_literal_type, int>(true), "");
  static_assert(test_category<is_literal_type, unsigned char>(true), "");

  static_assert(test_category<is_literal_type, TType>(true), "");
  static_assert(test_category<is_literal_type, PODType>(true), "");

  static_assert(test_category<is_literal_type, NType>(false), "");
  static_assert(test_category<is_literal_type, SLType>(false), "");

  static_assert(test_category<is_literal_type, LType>(true), "");
  static_assert(test_category<is_literal_type, LType[5]>(true), "");

  static_assert(test_category<is_literal_type, NLType>(false), "");
  static_assert(test_category<is_literal_type, NLType[5]>(false), "");

  static_assert(test_category<is_literal_type, LTypeDerived>(true), "");
  static_assert(test_category<is_literal_type, LTypeDerived[5]>(true), "");
}

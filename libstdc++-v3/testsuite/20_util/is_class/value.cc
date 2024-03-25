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
  using std::is_class;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_category<is_class, ClassType>(true), "");
  static_assert(test_category<is_class, IncompleteClass>(true), "");
  static_assert(test_category<is_class, DerivedType>(true), "");
  static_assert(test_category<is_class, ConvType>(true), "");
  static_assert(test_category<is_class, AbstractClass>(true), "");
  static_assert(test_category<is_class, PolymorphicClass>(true), "");
  static_assert(test_category<is_class, DerivedPolymorphic>(true), "");

  // Negative tests.
  static_assert(test_category<is_class, UnionType>(false), "");
  static_assert(test_category<is_class, void>(false), "");
  static_assert(test_category<is_class, int>(false), "");
  static_assert(test_category<is_class, float>(false), "");
  static_assert(test_category<is_class, int[2]>(false), "");
  static_assert(test_category<is_class, int*>(false), "");
  static_assert(test_category<is_class, int(*)(int)>(false), "");
  static_assert(test_category<is_class, float&>(false), "");
  static_assert(test_category<is_class, float(&)(float)>(false), "");
  static_assert(test_category<is_class, int (ClassType::*)>(false), "");
  static_assert(test_category<is_class, int (ClassType::*) (int)>(false), "");
  static_assert(test_category<is_class, int (int)>(false), "");
  static_assert(test_category<is_class, EnumType>(false), "");
  static_assert(test_category<is_class, IncompleteUnion>(false), "");
}

// { dg-do compile { target c++17 } }

// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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
#include <tuple>

template <class... T> void pos()
{
  static_assert((std::is_aggregate_v<T> &&...));
}

template <class... T> void neg()
{
  static_assert((!std::is_aggregate_v<T> &&...));
}

void test01()
{
  using namespace __gnu_test;
  using __gnu_test::test_category;
  using std::is_aggregate;

  // Positive tests.
  static_assert(test_category<is_aggregate,
		ClassType>(true), "");
  static_assert(test_category<is_aggregate,
		UnionType>(true), "");
  static_assert(test_category<is_aggregate,
		SLType>(true), "");
  static_assert(test_category<is_aggregate,
		unsigned[3]>(true), "");
  static_assert(test_category<is_aggregate,
		unsigned[3][2]>(true), "");
  static_assert(test_category<is_aggregate,
		unsigned[]>(true), "");
  static_assert(test_category<is_aggregate,
		unsigned[][2]>(true), "");
  static_assert(test_category<is_aggregate,
		EnumType[3]>(true), "");
  static_assert(test_category<is_aggregate,
		EnumType[3][2]>(true), "");
  static_assert(test_category<is_aggregate,
		EnumType[]>(true), "");
  static_assert(test_category<is_aggregate,
		EnumType[][2]>(true), "");
  pos<ClassType, UnionType, SLType,
      unsigned[3], unsigned[3][2], unsigned[], unsigned[][3]>();
#if __cplusplus == 201703L
  static_assert(test_category<is_aggregate,
		NoexceptMoveAssignClass>(true), "");
  pos<NoexceptMoveAssignClass>();
#endif

  // Negative tests.
  static_assert(test_category<is_aggregate,
		AbstractClass>(false), "");
  static_assert(test_category<is_aggregate,
		PolymorphicClass>(false), "");
  static_assert(test_category<is_aggregate,
		ExplicitClass>(false), "");
  static_assert(test_category<is_aggregate,
		char>(false), "");
  static_assert(test_category<is_aggregate,
		unsigned char>(false), "");
  static_assert(test_category<is_aggregate,
		signed char>(false), "");
  static_assert(test_category<is_aggregate,
		unsigned>(false), "");
  static_assert(test_category<is_aggregate,
                bool>(false), "");
  static_assert(test_category<is_aggregate,
                float>(false), "");
  static_assert(test_category<is_aggregate,
                double>(false), "");
  static_assert(test_category<is_aggregate,
		EnumType>(false), "");
  static_assert(test_category<is_aggregate,
		void>(false), "");
  neg<AbstractClass, PolymorphicClass, ExplicitClass, char, unsigned,
      bool, float, double, void>();
#if __cplusplus > 201703L
  // In C++20 aggregates cannot have user-declared constructors.
  static_assert(test_category<is_aggregate,
		NoexceptMoveAssignClass>(false), "");
  neg<NoexceptMoveAssignClass>();
#endif
}

// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <type_traits>
#include <testsuite_tr1.h>

using std::is_nothrow_convertible;

void test01()
{
  using namespace __gnu_test;

  // Positive conversion tests.
  static_assert(test_relationship<is_nothrow_convertible,
				  int, int>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  int, const int>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  volatile int, const int>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  int, float>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  double, float>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  float, int>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  int*, const int*>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  int*, void*>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  int[4], int*>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  float&, int>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  int, const int&>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  const int&, int>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  float, const int&>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  int(int), int(*)(int)>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  int(&)(int), int(*)(int)>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  EnumType, int>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  ClassType, ClassType>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  DerivedType, ClassType>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  DerivedType*, ClassType*>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  DerivedType&, ClassType&>(true));

  static_assert(test_relationship<is_nothrow_convertible,
				  const int, const int&>(true));

  static_assert(test_relationship<is_nothrow_convertible,
				  void, void>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  const void, void>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  void, volatile void>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  double&, NoexceptExplicitClass>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  NoexceptCopyConsClass,
				  NoexceptCopyConsClass>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  const NoexceptCopyConsClass,
				  NoexceptCopyConsClass>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  const NoexceptCopyConsClass&,
				  NoexceptCopyConsClass>(true));
  static_assert(test_relationship<is_nothrow_convertible,
				  NoexceptMoveConsClass,
				  NoexceptMoveConsClass>(true));

  static_assert(test_relationship<is_nothrow_convertible,
				  int(int), int(&)(int)>(true));

  // Negative conversion tests.
  static_assert(test_relationship<is_nothrow_convertible,
				  const int*, int*>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  int*, float*>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  const int[4], int*>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  int[4], int[4]>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  const int&, int&>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  float&, int&>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  float, volatile int&>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  int(int), int(int)>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  int(int), int(*)(void)>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  int(*)(int), int(&)(int)>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  int, EnumType>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  int, ClassType>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  ClassType, DerivedType>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  ClassType*, DerivedType*>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  ClassType&, DerivedType&>(false));

  static_assert(test_relationship<is_nothrow_convertible,
				  void, int>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  void, float>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  void, int(*)(int)>(false));

  // C++0x
  static_assert(test_relationship<is_nothrow_convertible,
				  int, void>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  int[4], void>(false));

  static_assert(test_relationship<is_nothrow_convertible,
				  int, int&>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  float, volatile float&>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  const volatile int,
				  const volatile int&>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  volatile int, volatile int&>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  double&, ExplicitClass>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  int&, ExplicitClass>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  void*, ExplicitClass>(false));

  static_assert(test_relationship<is_nothrow_convertible,
				  ExceptCopyConsClass,
				  ExceptCopyConsClass>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  const ExceptCopyConsClass,
				  ExceptCopyConsClass>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  const ExceptCopyConsClass&,
				  ExceptCopyConsClass>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  ExceptMoveConsClass,
				  ExceptMoveConsClass>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  ExceptMoveConsClass&,
				  ExceptMoveConsClass>(false));
  static_assert(test_relationship<is_nothrow_convertible,
				  NoexceptMoveConsClass&,
				  NoexceptMoveConsClass>(false));
}

void test02()
{
  struct X { };

  struct Y
  {
    explicit Y(X) noexcept; // not viable for implicit conversions
    Y(...);
  };

  static_assert(!is_nothrow_convertible<X, Y>::value, "");
}

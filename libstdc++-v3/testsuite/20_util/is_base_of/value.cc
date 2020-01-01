// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

class HiddenCons
{
  HiddenCons();
  HiddenCons(const HiddenCons&);
};

class DerivedHiddenCons
: private HiddenCons
{
  DerivedHiddenCons();
  DerivedHiddenCons(const DerivedHiddenCons&);
};

class MultiDerivedHiddenCons
: private HiddenCons, private __gnu_test::ClassType 
{
  MultiDerivedHiddenCons();
  MultiDerivedHiddenCons(const MultiDerivedHiddenCons&);
};

void test01()
{
  using std::is_base_of;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_relationship<is_base_of, volatile ClassType,
		ClassType>(true), "");
  static_assert(test_relationship<is_base_of, AbstractClass,
		AbstractClass>(true), "");
  static_assert(test_relationship<is_base_of, ClassType,
		DerivedType>(true), "");
  static_assert(test_relationship<is_base_of, ClassType,
		const DerivedType>(true), "");
  static_assert(test_relationship<is_base_of, volatile ClassType,
		volatile DerivedType>(true), "");
  static_assert(test_relationship<is_base_of, PolymorphicClass,
		DerivedPolymorphic>(true), "");
  static_assert(test_relationship<is_base_of, HiddenCons,
		DerivedHiddenCons>(true), "");
  static_assert(test_relationship<is_base_of, HiddenCons,
		MultiDerivedHiddenCons>(true), "");
  static_assert(test_relationship<is_base_of, ClassType,
		MultiDerivedHiddenCons>(true), "");

  // Negative tests.
  static_assert(test_relationship<is_base_of, int, int>(false), "");
  static_assert(test_relationship<is_base_of, EnumType, EnumType>(false), "");
  static_assert(test_relationship<is_base_of, UnionType, UnionType>(false), "");
  static_assert(test_relationship<is_base_of, int, const int>(false), "");
  static_assert(test_relationship<is_base_of, volatile UnionType,
		UnionType>(false), "");
  static_assert(test_relationship<is_base_of, int&, ClassType>(false), "");
  static_assert(test_relationship<is_base_of, AbstractClass,
		ClassType>(false), "");
  static_assert(test_relationship<is_base_of, ClassType,
		AbstractClass>(false), "");
  static_assert(test_relationship<is_base_of, DerivedType,
		ClassType>(false), "");
  static_assert(test_relationship<is_base_of, DerivedPolymorphic,
		PolymorphicClass>(false), "");
  static_assert(test_relationship<is_base_of, DerivedHiddenCons,
		HiddenCons>(false), "");
  static_assert(test_relationship<is_base_of, MultiDerivedHiddenCons,
		HiddenCons>(false), "");
  static_assert(test_relationship<is_base_of, MultiDerivedHiddenCons,
		ClassType>(false), "");
}

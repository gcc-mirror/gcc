// { dg-do compile { target c++11 } }
//
// 2014-10-09  Ville Voutilainen  <ville.voutilainen@gmail.com>
//
// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

struct HasTemplateCCtor
{
  HasTemplateCCtor(const HasTemplateCCtor&) = default;
  template <class T>
  HasTemplateCCtor(T&&);
};

struct MoveOnly
{
  MoveOnly(MoveOnly&&) = default;
};

struct MoveOnly2
{
  MoveOnly2(MoveOnly2&&) = delete;
};

void test01()
{
  using std::is_trivially_constructible;
  using namespace __gnu_test;

  static_assert(test_property<is_trivially_constructible,
		int>(true), "");
  static_assert(test_property<is_trivially_constructible,
		int, int>(true), "");
  static_assert(test_property<is_trivially_constructible,
		int, int&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		int, int&&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		int, const int&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		int, void*>(false), "PR 86398");
  static_assert(test_property<is_trivially_constructible,
		int, int*>(false), "PR 86398");
  static_assert(test_property<is_trivially_constructible,
		int, const int*>(false), "PR 86398");
  static_assert(test_property<is_trivially_constructible,
		int*, void*>(false), "PR 86398");
  static_assert(test_property<is_trivially_constructible,
		int*, const int*>(false), "PR 86398");
  static_assert(test_property<is_trivially_constructible,
		int&, const int>(false), "");
  static_assert(test_property<is_trivially_constructible,
		const int&, int>(true), "");
  static_assert(test_property<is_trivially_constructible,
		const int&, int&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		const int*, int*>(true), "");
  static_assert(test_property<is_trivially_constructible,
		PolymorphicClass>(false), "");
  static_assert(test_property<is_trivially_constructible,
		PolymorphicClass, PolymorphicClass>(false), "");
  static_assert(test_property<is_trivially_constructible,
		PolymorphicClass, PolymorphicClass&>(false), "");
  static_assert(test_property<is_trivially_constructible,
		PolymorphicClass, PolymorphicClass&&>(false), "");
  static_assert(test_property<is_trivially_constructible,
		PolymorphicClass, const PolymorphicClass&>(false), "");
  static_assert(test_property<is_trivially_constructible,
		TType>(true), "");
  static_assert(test_property<is_trivially_constructible,
		TType, TType>(true), "");
  static_assert(test_property<is_trivially_constructible,
		TType, TType&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		TType, TType&&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		TType, const TType&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		TType, int, int>(false), "");
  static_assert(test_property<is_trivially_constructible,
		PODType>(true), "");
  static_assert(test_property<is_trivially_constructible,
		PODType, PODType>(true), "");
  static_assert(test_property<is_trivially_constructible,
		PODType, PODType&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		PODType, PODType&&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		PODType, const PODType&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		PODType, int, int>(false), "");
  static_assert(test_property<is_trivially_constructible,
		NType>(false), "");
  static_assert(test_property<is_trivially_constructible,
		SLType>(false), "");
  static_assert(test_property<is_trivially_constructible,
		LType>(false), "");
  static_assert(test_property<is_trivially_constructible,
		LType, int>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::DelDef>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::Abstract>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::Ellipsis>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::DelEllipsis>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::Any>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::DelCopy>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::DelCopy, const construct::DelCopy&>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::DelDtor>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::Nontrivial>(false), "");
  static_assert(test_property<is_trivially_constructible,
		construct::UnusualCopy>(false), "");
  static_assert(test_property<is_trivially_constructible,
		CopyConsOnlyType>(false), "");
  static_assert(test_property<is_trivially_constructible,
		CopyConsOnlyType, CopyConsOnlyType>(false), "");
  static_assert(test_property<is_trivially_constructible,
		CopyConsOnlyType, CopyConsOnlyType&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		CopyConsOnlyType, CopyConsOnlyType&&>(false), "");
  static_assert(test_property<is_trivially_constructible,
		CopyConsOnlyType, const CopyConsOnlyType&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		MoveConsOnlyType>(false), "");
  static_assert(test_property<is_trivially_constructible,
		MoveConsOnlyType, MoveConsOnlyType>(true), "");
  static_assert(test_property<is_trivially_constructible,
		MoveConsOnlyType, MoveConsOnlyType&>(false), "");
  static_assert(test_property<is_trivially_constructible,
		MoveConsOnlyType, MoveConsOnlyType&&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		MoveConsOnlyType, const MoveConsOnlyType&>(false), "");
  static_assert(test_property<is_trivially_constructible,
		ClassType, DerivedType>(true), "");
  static_assert(test_property<is_trivially_constructible,
		ClassType, DerivedType&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		ClassType, DerivedType&&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		ClassType, const DerivedType&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		HasTemplateCCtor>(false), "");
  static_assert(test_property<is_trivially_constructible,
		HasTemplateCCtor, HasTemplateCCtor>(false), "");
  static_assert(test_property<is_trivially_constructible,
		HasTemplateCCtor, const HasTemplateCCtor&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		MoveOnly>(false), "");
  static_assert(test_property<is_trivially_constructible,
		MoveOnly, MoveOnly>(true), "");
  static_assert(test_property<is_trivially_constructible,
		MoveOnly, MoveOnly&>(false), "");
  static_assert(test_property<is_trivially_constructible,
		MoveOnly, MoveOnly&&>(true), "");
  static_assert(test_property<is_trivially_constructible,
		MoveOnly, const MoveOnly&>(false), "");
  static_assert(test_property<is_trivially_constructible,
		MoveOnly2>(false), "");
  static_assert(test_property<is_trivially_constructible,
		int[]>(false), "PR c++/90532");
}

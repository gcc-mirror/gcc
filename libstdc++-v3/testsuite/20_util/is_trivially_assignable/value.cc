// { dg-do compile { target c++11 } }
//
// 2014-10-09  Ville Voutilainen  <ville.voutilainen@gmail.com>
//
// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

struct HasTemplateCAssign
{
  HasTemplateCAssign& operator=(const HasTemplateCAssign&) = default;
  template <class T>
  HasTemplateCAssign& operator=(T&&);
};

struct MoveOnly
{
  MoveOnly& operator=(MoveOnly&&) = default;
};

struct MoveOnly2
{
  MoveOnly2& operator=(MoveOnly2&&) = delete;
};

void test01()
{
  using std::is_trivially_assignable;
  using namespace __gnu_test;

  static_assert(test_property<is_trivially_assignable,
		int, int>(false), "");
  static_assert(test_property<is_trivially_assignable,
		int&, int>(true), "");
  static_assert(test_property<is_trivially_assignable,
		int&, int&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		int&, int&&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		int&, const int&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		int&, int*>(false), "");
  static_assert(test_property<is_trivially_assignable,
		int&, void*>(false), "");
  static_assert(test_property<is_trivially_assignable,
		const int, int>(false), "");
  static_assert(test_property<is_trivially_assignable,
		const int&, int>(false), "");
  static_assert(test_property<is_trivially_assignable,
		const int&, const int&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		const int*&, int*>(true), "");
  static_assert(test_property<is_trivially_assignable,
		int*&, const int*&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		int*&, const int&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		const int*&, void*>(false), "");
  static_assert(test_property<is_trivially_assignable,
		const void*&, void*>(true), "");
  static_assert(test_property<is_trivially_assignable,
		const void*&, int*>(true), "");

  static_assert(test_property<is_trivially_assignable,
		TType, TType>(true), "");
  static_assert(test_property<is_trivially_assignable,
		TType&, TType>(true), "");
  static_assert(test_property<is_trivially_assignable,
		TType&, TType&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		TType&, TType&&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		TType&, const TType&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		PODType, PODType>(true), "");
  static_assert(test_property<is_trivially_assignable,
		NType&, NType&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		SLType, SLType>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::Empty, assign::Empty>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::Abstract, assign::Abstract>(false), "");
  static_assert(test_property<is_trivially_assignable,
		assign::Ellipsis, assign::Ellipsis>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::DelEllipsis, assign::DelEllipsis>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::Any, assign::Any>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::DelDef, assign::DelDef>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::DelCopy, assign::DelCopy>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::Nontrivial, assign::Nontrivial>(false), "");
  static_assert(test_property<is_trivially_assignable,
		assign::AnyAssign, assign::AnyAssign>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::DelAnyAssign, assign::DelAnyAssign>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::DelCopyAssign, assign::DelCopyAssign>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::MO, assign::MO>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::MO, assign::MO&&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		assign::MO, assign::MO&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		assign::MO, const assign::MO&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		CopyConsOnlyType, CopyConsOnlyType>(false), "");
  static_assert(test_property<is_trivially_assignable,
		CopyConsOnlyType, const CopyConsOnlyType&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		MoveConsOnlyType, MoveConsOnlyType>(false), "");
  static_assert(test_property<is_trivially_assignable,
		MoveConsOnlyType, MoveConsOnlyType&&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		HasTemplateCAssign, HasTemplateCAssign>(false), "");
  static_assert(test_property<is_trivially_assignable,
		HasTemplateCAssign, const HasTemplateCAssign&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		ClassType, DerivedType>(true), "");
  static_assert(test_property<is_trivially_assignable,
		ClassType, DerivedType&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		ClassType, DerivedType&&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		ClassType, const DerivedType&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		MoveOnly, MoveOnly>(true), "");
  static_assert(test_property<is_trivially_assignable,
		MoveOnly, MoveOnly&&>(true), "");
  static_assert(test_property<is_trivially_assignable,
		MoveOnly, MoveOnly&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		MoveOnly, const MoveOnly&>(false), "");
  static_assert(test_property<is_trivially_assignable,
		MoveOnly2, MoveOnly2>(false), "");
}

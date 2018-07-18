// { dg-do compile { target c++11 } }
//
// 2014-10-09  Ville Voutilainen  <ville.voutilainen@gmail.com>
//
// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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
  using std::is_trivially_copy_assignable;
  using namespace __gnu_test;

  static_assert(test_property<is_trivially_copy_assignable, 
		int>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		TType>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		PODType>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		NType>(false), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		SLType>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::Empty>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::Abstract>(false), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::Ellipsis>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::DelEllipsis>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::Any>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::DelDef>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::DelCopy>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::Nontrivial>(false), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::AnyAssign>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::DelAnyAssign>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::DelCopyAssign>(false), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		assign::MO>(false), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		CopyConsOnlyType>(false), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		MoveConsOnlyType>(false), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		HasTemplateCAssign>(true), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		MoveOnly>(false), "");
  static_assert(test_property<is_trivially_copy_assignable, 
		MoveOnly2>(false), "");
  static_assert(test_property<is_trivially_copy_assignable,
		void>(false), "");
}

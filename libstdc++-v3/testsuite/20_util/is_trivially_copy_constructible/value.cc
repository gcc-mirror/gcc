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
  using std::is_trivially_copy_constructible;
  using namespace __gnu_test;

  static_assert(test_property<is_trivially_copy_constructible, 
		int>(true), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		TType>(true), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		PODType>(true), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		NType>(false), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		SLType>(false), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		construct::DelDef>(true), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		construct::Abstract>(false), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		construct::Ellipsis>(true), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		construct::DelEllipsis>(true), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		construct::Any>(true), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		construct::DelCopy>(false), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		construct::DelDtor>(false), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		construct::Nontrivial>(false), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		construct::UnusualCopy>(false), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		CopyConsOnlyType>(true), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		MoveConsOnlyType>(false), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		HasTemplateCCtor>(true), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		MoveOnly>(false), "");
  static_assert(test_property<is_trivially_copy_constructible, 
		MoveOnly2>(false), "");
  static_assert(test_property<is_trivially_copy_constructible,
		void>(false), "");
}

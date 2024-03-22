// { dg-do compile { target c++11 } }
//
// 2014-10-09  Ville Voutilainen  <ville.voutilainen@gmail.com>
//
// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

struct HasTemplateCtor
{
  HasTemplateCtor() = default;
  template <class T>
  HasTemplateCtor();
};

void test01()
{
  using std::is_trivially_default_constructible;
  using namespace __gnu_test;

  static_assert(test_category<is_trivially_default_constructible, 
		int>(true), "");
  static_assert(test_category<is_trivially_default_constructible, 
		TType>(true), "");
  static_assert(test_category<is_trivially_default_constructible, 
		PODType>(true), "");
  static_assert(test_category<is_trivially_default_constructible, 
		NType>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		SLType>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		construct::DelDef>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		construct::Abstract>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		construct::Ellipsis>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		construct::DelEllipsis>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		construct::Any>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		construct::DelCopy>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		construct::DelDtor>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		construct::Nontrivial>(false), "");
  static_assert(test_category<is_trivially_default_constructible, 
		HasTemplateCtor>(true), "");

  static_assert(test_category<is_trivially_default_constructible,
		int[]>(false), "PR c++/90532");
  struct A { };
  static_assert(test_category<is_trivially_default_constructible,
		A[]>(false), "PR c++/90532");
  struct B { B() { } };
  static_assert(test_category<is_trivially_default_constructible,
		B[]>(false), "PR c++/90532");
}

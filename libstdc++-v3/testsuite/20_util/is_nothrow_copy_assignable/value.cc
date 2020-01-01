// { dg-do compile { target c++11 } }

// 2010-06-08  Paolo Carlini  <paolo.carlini@oracle.com>
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
  using std::is_nothrow_copy_assignable;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_property<is_nothrow_copy_assignable, int>(true), "");
  static_assert(test_property<is_nothrow_copy_assignable, float>(true), "");
  static_assert(test_property<is_nothrow_copy_assignable, EnumType>(true), "");
  static_assert(test_property<is_nothrow_copy_assignable, int*>(true), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		int(*)(int)>(true), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		int (ClassType::*)>(true), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		int (ClassType::*) (int)>(true), "");

  static_assert(test_property<is_nothrow_copy_assignable,
		NoexceptCopyAssignClass>(true), "");

  // Negative tests.
  static_assert(test_property<is_nothrow_copy_assignable, void>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable, int[2]>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		float[][3]>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		EnumType[2][3][4]>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable, int*[3]>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		int(*[][2])(int)>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		int (ClassType::*[2][3])>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable, 
		int (ClassType::*[][2][3]) (int)>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		ClassType(unsigned) const &>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		bool(ClassType) const>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable, 
		bool(...) &&>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable, 
		EnumType(int, ...)>(false), "");

  static_assert(test_property<is_nothrow_copy_assignable,
		ExceptCopyAssignClass>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		NoexceptMoveAssignClass>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		ExceptMoveAssignClass>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		DeletedCopyAssignClass>(false), "");
  static_assert(test_property<is_nothrow_copy_assignable,
		DeletedMoveAssignClass>(false), "");
}

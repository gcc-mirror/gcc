// { dg-do compile { target c++11 } }

// 2011-05-19  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2021 Free Software Foundation, Inc.
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
  using std::is_nothrow_move_assignable;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_property<is_nothrow_move_assignable, int>(true), "");
  static_assert(test_property<is_nothrow_move_assignable, float>(true), "");
  static_assert(test_property<is_nothrow_move_assignable, EnumType>(true), "");
  static_assert(test_property<is_nothrow_move_assignable, int*>(true), "");
  static_assert(test_property<is_nothrow_move_assignable,
		int(*)(int)>(true), "");
  static_assert(test_property<is_nothrow_move_assignable,
		int (ClassType::*)>(true), "");
  static_assert(test_property<is_nothrow_move_assignable,
		int (ClassType::*) (int)>(true), "");

  static_assert(test_property<is_nothrow_move_assignable,
		NoexceptMoveAssignClass>(true), "");
  static_assert(test_property<is_nothrow_move_assignable,
		NoexceptCopyAssignClass>(true), "");

  // Negative tests.
  static_assert(test_property<is_nothrow_move_assignable, void>(false), "");
  static_assert(test_property<is_nothrow_move_assignable, int[2]>(false), "");
  static_assert(test_property<is_nothrow_move_assignable,
		float[][3]>(false), "");
  static_assert(test_property<is_nothrow_move_assignable,
		EnumType[2][3][4]>(false), "");
  static_assert(test_property<is_nothrow_move_assignable, int*[3]>(false), "");
  static_assert(test_property<is_nothrow_move_assignable,
		int(*[][2])(int)>(false), "");
  static_assert(test_property<is_nothrow_move_assignable,
		int (ClassType::*[2][3])>(false), "");
  static_assert(test_property<is_nothrow_move_assignable, 
		int (ClassType::*[][2][3]) (int)>(false), "");
  static_assert(test_property<is_nothrow_move_assignable,
		ClassType(unsigned) const &>(false), "");
  static_assert(test_property<is_nothrow_move_assignable,
		bool(ClassType) const>(false), "");
  static_assert(test_property<is_nothrow_move_assignable, 
		bool(...) &&>(false), "");
  static_assert(test_property<is_nothrow_move_assignable, 
		EnumType(int, ...)>(false), "");

  static_assert(test_property<is_nothrow_move_assignable,
		ExceptMoveAssignClass>(false), "");
  static_assert(test_property<is_nothrow_move_assignable,
		ExceptCopyAssignClass>(false), "");
  static_assert(test_property<is_nothrow_move_assignable,
		DeletedMoveAssignClass>(false), "");
  static_assert(test_property<is_nothrow_move_assignable,
		DeletedCopyAssignClass>(false), "");
}

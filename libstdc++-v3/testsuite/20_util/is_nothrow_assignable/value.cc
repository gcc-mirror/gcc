// { dg-do compile { target c++11 } }

// 2011-05-19  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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
  using std::is_nothrow_assignable;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_relationship<is_nothrow_assignable, int&, int>(true), "");
  static_assert(test_relationship<is_nothrow_assignable, int&,
		const int>(true), "");

  static_assert(test_relationship<is_nothrow_assignable,
		NoexceptCopyAssignClass&,
		const NoexceptCopyAssignClass&>(true), "");
  static_assert(test_relationship<is_nothrow_assignable,
		NoexceptMoveAssignClass&, NoexceptMoveAssignClass&&>(true), "");
  static_assert(test_relationship<is_nothrow_assignable,
		NoexceptCopyAssignClass&, NoexceptCopyAssignClass&&>(true), "");

  // Negative tests.
  static_assert(test_relationship<is_nothrow_assignable, int, int>(false), "");
  static_assert(test_relationship<is_nothrow_assignable, int,
		const int>(false), "");

  static_assert(test_relationship<is_nothrow_assignable,
		ExceptCopyAssignClass&,
		const ExceptCopyAssignClass&>(false), "");
  static_assert(test_relationship<is_nothrow_assignable,
		ExceptMoveAssignClass&, ExceptMoveAssignClass&&>(false), "");
  static_assert(test_relationship<is_nothrow_assignable,
		NoexceptMoveAssignClass&,
		const NoexceptMoveAssignClass&>(false), "");
}

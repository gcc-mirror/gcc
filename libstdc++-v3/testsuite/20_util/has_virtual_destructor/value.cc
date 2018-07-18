// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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
#include <iostream>
#include <testsuite_tr1.h>

void test01()
{
  using std::has_virtual_destructor;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_category<has_virtual_destructor,
		VirtualDestructorClass>(true), "");
  static_assert(test_category<has_virtual_destructor, std::iostream>(true), "");
  static_assert(test_category<has_virtual_destructor,
		std::streambuf>(true), "");

  // Negative tests.
  static_assert(test_category<has_virtual_destructor,
		PolymorphicClass>(false), "");
  static_assert(test_category<has_virtual_destructor,
		DerivedPolymorphic>(false), "");
  static_assert(test_category<has_virtual_destructor,
		AbstractClass>(false), "");
  static_assert(test_category<has_virtual_destructor, void>(false), "");
  static_assert(test_category<has_virtual_destructor, int (int)>(false), "");
  static_assert(test_category<has_virtual_destructor, int&>(false), "");
  static_assert(test_category<has_virtual_destructor, EnumType>(false), "");
  static_assert(test_category<has_virtual_destructor, ClassType>(false), "");
  static_assert(test_category<has_virtual_destructor, DerivedType>(false), "");
}

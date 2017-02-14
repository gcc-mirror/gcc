// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2017 Free Software Foundation, Inc.
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
  using std::add_pointer;
  using std::is_same;
  using namespace __gnu_test;

  static_assert(is_same<add_pointer<int>::type, int*>::value, "");
  static_assert(is_same<add_pointer<int*>::type, int**>::value, "");
  static_assert(is_same<add_pointer<const int>::type, const int*>::value, "");
  static_assert(is_same<add_pointer<int&>::type, int*>::value, "");
  static_assert(is_same<add_pointer<ClassType*>::type,
		ClassType**>::value, "");
  static_assert(is_same<add_pointer<ClassType>::type, ClassType*>::value, "");
}

void test02()
{
  using std::add_pointer;
  using std::is_same;

  void f1();
  using f1_type = decltype(f1);
  using pf1_type = decltype(&f1);
  static_assert(is_same<add_pointer<f1_type>::type, pf1_type>::value, "");
  void f2() noexcept; // PR libstdc++/78361
  using f2_type = decltype(f2);
  using pf2_type = decltype(&f2);
  static_assert(is_same<add_pointer<f2_type>::type, pf2_type>::value, "");
}

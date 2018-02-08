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
#include <testsuite_tr1.h>

class EmptyClassOne
{ typedef int type; };

class EmptyClassTwo
{ static int data; };

class EmptyClassThree
{ int f(); };

class NonEmptyClassOne
{ int data; };

class NonEmptyClassTwo
{
  virtual int f();
  virtual ~NonEmptyClassTwo();
};

void test01()
{
  using std::is_empty;
  using namespace __gnu_test;

  // Positive tests.
  static_assert(test_category<is_empty, ClassType>(true), "");
  static_assert(test_category<is_empty, EmptyClassOne>(true), "");
  static_assert(test_category<is_empty, EmptyClassTwo>(true), "");
  static_assert(test_category<is_empty, EmptyClassThree>(true), "");

  // Negative tests.
  static_assert(test_category<is_empty, void>(false), "");
  static_assert(test_category<is_empty, float>(false), "");
  static_assert(test_category<is_empty, int[4]>(false), "");
  static_assert(test_category<is_empty, int*>(false), "");
  static_assert(test_category<is_empty, int&>(false), "");
  static_assert(test_category<is_empty, int (ClassType::*)>(false), "");
  static_assert(test_category<is_empty, EnumType>(false), "");
  static_assert(test_category<is_empty, int (int)>(false), "");

  static_assert(test_category<is_empty, AbstractClass>(false), "");
  static_assert(test_category<is_empty, NonEmptyClassOne>(false), "");
  static_assert(test_category<is_empty, NonEmptyClassTwo>(false), "");  
}

// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <functional>
#include <type_traits>

struct X {
  int i;
  void f1() { }
  void f2() volatile { }
};

typedef int X::*pm;
typedef void (X::*pmf1)();
typedef void (X::*pmf2)() volatile;

typedef std::result_of<pm const&(X&)>::type result;
static_assert(std::is_same<result, int&>::value,
              "invoking cv-qualified pointer-to-member-object");

typedef std::result_of<pmf1 const&(X&)>::type result1;
static_assert(std::is_void<result1>::value,
              "invoking cv-qualified pointer-to-member-function");

typedef std::result_of<pmf2 const&(X&)>::type result2;
static_assert(std::is_void<result2>::value,
              "invoking cv-qualified pointer-to-member-function");

typedef std::result_of<pm(volatile X&)>::type result3;
static_assert(std::is_same<result3, volatile int&>::value,
              "invoking pointer-to-member-object on volatile object");

typedef std::result_of<pmf2(volatile X&)>::type result4;
static_assert(std::is_void<result4>::value,
              "invoking pointer-to-member-function on volatile object");


// { dg-options "-std=gnu++0x" }
// { dg-do compile }
// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 20.6.4 function object return types [func.ret]
#include <functional>
#include <testsuite_hooks.h>

struct X
{
    int i;
    int f();
};

void test01()
{
  bool test __attribute__((unused)) = true;

  using std::result_of;
  using std::is_same;

  typedef int X::*pm;
  typedef int (X::*pmf)();
  typedef int (*pf)();

  result_of<pmf(X*, int)>::type test2; // { dg-error "here" }
  // { dg-error "too many arguments to function" "" { target *-*-* } 286 }
  result_of<pf(int)>::type test3; // { dg-error "here" }
  // { dg-error "too many arguments to function" "" { target *-*-* } 299 }
}

// { dg-excess-errors "" }

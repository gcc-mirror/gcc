// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }
// { dg-require-atomic-builtins "" }

// Copyright (C) 2009 Free Software Foundation, Inc.
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


#include <future>
#include <testsuite_tr1.h>

void test01()
{
  using std::shared_future;
  using namespace __gnu_test;

  shared_future<int> p1;            // { dg-error "22: error: no match" }
  shared_future<int&> p2;           // { dg-error "23: error: no match" }
  shared_future<void> p3;           // { dg-error "23: error: no match" }
  shared_future<ClassType> p4;      // { dg-error "28: error: no match" }
  shared_future<AbstractClass&> p5; // { dg-error "33: error: no match" }
}

int main()
{
  test01();
  return 0;
}
// { dg-excess-errors "note" }

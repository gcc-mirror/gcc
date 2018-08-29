// { dg-do compile { target c++14 } }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

// 8.2.1 Class template shared_ptr [memory.smartptr.shared]

#include <experimental/memory>

struct A { };
struct B : A { };
struct D
{
  void operator()(A* p) { delete [] p; ++delete_count; }
  static long delete_count;
};
long D::delete_count = 0;

// C++14 §20.8.2.2.4

// reset
void
test01()
{
  std::experimental::shared_ptr<A[5]> p1(new A[5]);
  p1.reset(new B[1]);           // { dg-error "no matching function" }
  p1.reset(new B[5], D());      // { dg-error "no matching function" }
  using constA = const A;
  p1.reset(new constA[5]);      // { dg-error "no matching function" }
  p1.reset(new constA[5], D()); // { dg-error "no matching function" }
}

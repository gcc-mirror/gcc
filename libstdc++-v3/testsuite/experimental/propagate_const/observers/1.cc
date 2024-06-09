// { dg-do run { target c++14 } }
// { dg-options "-fdelete-null-pointer-checks" }

// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

#include <experimental/propagate_const>
#include <testsuite_hooks.h>

using std::experimental::propagate_const;
using std::hash;

int main()
{
  int x{42};
  propagate_const<int*> xx{&x};
  VERIFY(bool(xx));
  propagate_const<int*> xx2{};
  VERIFY(!bool(xx2));
  struct X {int x;};
  X x3{42};
  propagate_const<X*> xx3{&x3};
  VERIFY(xx3->x == 42);
  VERIFY((*xx3).x == 42);
  VERIFY(xx3.get() == &x3);
  const propagate_const<X*> xx4{&x3};
  VERIFY(xx4->x == 42);
  VERIFY((*xx4).x == 42);
  VERIFY(xx4.get() == &x3);
  static constexpr int x4{42};
  constexpr propagate_const<const int*> xx5{&x4};
  static_assert(bool(xx5), "");
  constexpr propagate_const<const int*> xx6{};
  static_assert(!bool(xx6), "");
  struct X2 {int x;};
  static constexpr X2 x5{42};
  constexpr propagate_const<const X2*> xx7{&x5};
  static_assert(xx7->x == 42, "");
  static_assert((*xx7).x == 42, "");
  static_assert(xx7.get() == &x5, "");
  struct X3
  {
    int f() {return 42;}
    int f() const {return 666;}
  };
  X3 xx8;
  propagate_const<X3*> xx9{&xx8};
  const propagate_const<X3*> xx10{&xx8};
  VERIFY(xx9->f() == 42);
  VERIFY(xx10->f() == 666);
}

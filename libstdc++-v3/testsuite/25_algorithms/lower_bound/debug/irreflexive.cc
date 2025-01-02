// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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
//
// { dg-do compile }
// { dg-require-debug-mode "" }

#include <algorithm>

struct A
{
  A(int i) : _i(i)
  { }

  int _i;
};

// Only knowns how to compare an A with an int.
struct A_int_comparer
{
  bool
  operator()(A a, int i) const
  { return a._i < i; }

  bool
  operator()(int i, A a) const
  { return i < a._i; }
};

void test01()
{
  A as[] = { 0, 1, 2, 3 };
  std::lower_bound(as, as + 4, 1, A_int_comparer());
  // { dg-warning "ignoring return value" "" { target c++11 } 46 }
}

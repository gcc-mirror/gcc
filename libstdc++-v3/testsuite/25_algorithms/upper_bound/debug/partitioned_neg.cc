// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do run { xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <algorithm>

struct A
{
  A(int i) : _i(i)
  { }

  int _i;

  bool
  operator<(const A& a) const
  { return _i < a._i; }
};

void test01()
{
  A as[] = { 0, 2, 1, 3, 4, 5 };
  std::upper_bound(as, as + 6, A(1));
  // { dg-warning "ignoring return value" "" { target c++11 } 38 }
}

int
main()
{
  test01();
  return 0;
}

// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

// 20.7.11 Function template bind

// PR c++/57899
// { dg-do compile { target c++11 } }

#include <functional>
using std::bind;
using std::placeholders::_1;

struct S { int i; };

struct P { S s; };

struct get_s
{
  const S& operator()(const P& p) const { return p.s; }
} gs;

int gi(const S& s) { return s.i; }

bool cmp(int, int) { return true; }

int main()
{
  P p{};
  auto f1 = bind(gs, _1);
  auto f2 = bind(gi, f1);
  auto f3 = bind(cmp, f2, 5);
  f3(p);
}

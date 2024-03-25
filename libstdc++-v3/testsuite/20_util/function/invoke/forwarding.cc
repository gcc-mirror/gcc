// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

#include <functional>
#include <testsuite_hooks.h>

struct Counted
{
  Counted() : count(0) { }
  Counted(const Counted& c) : count(c.count + 1) { }
  int count;
};

int func(Counted c) { return c.count; }

std::function<int(Counted)> f = func;

void
test01()
{
  Counted c;
  int n = f(c);
  // 1 copy invoking function::operator() and 1 copy invoking func
  VERIFY( n == 2 );
}

void
test02()
{
  int n = f(Counted{});
  // copy elided when invoking function::operator(), 1 copy invoking func
  VERIFY( n == 1 );
}

int
main()
{
  test01();
  test02();
}

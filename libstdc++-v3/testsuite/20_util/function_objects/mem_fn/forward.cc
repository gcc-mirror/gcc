// { dg-do run { target c++11 } }

// Copyright (C) 2012-2019 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

struct Counter
{
  Counter() = default;
  Counter(const Counter&) { ++count; }

  static int count;
};

int Counter::count = 0;

struct X
{
  int func(Counter, int i) { return i; }
  char func_c(Counter, char c) const { return c; }
  short func_v(Counter, short s) volatile { return s; }
  double func_cv(Counter, double d) const volatile { return d; }
};

void test01()
{
  Counter c;
  X x;

  std::mem_fn( &X::func )( x, c, 0 );
  VERIFY( Counter::count == 1 );

  std::mem_fn( &X::func_c )( x, c, 0 );
  VERIFY( Counter::count == 2 );

  std::mem_fn( &X::func_v )( x, c, 0 );
  VERIFY( Counter::count == 3 );

  std::mem_fn( &X::func_cv )( x, c, 0 );
  VERIFY( Counter::count == 4 );
}

int main()
{
  test01();
}

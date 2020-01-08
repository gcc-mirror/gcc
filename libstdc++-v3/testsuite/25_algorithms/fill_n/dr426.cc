// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

// { dg-do run }

#include <algorithm>
#include <testsuite_hooks.h>

// DR 426. search_n(), fill_n(), and generate_n() with negative n

void
test01()
{
  int i = 99;
  std::fill_n(&i, 0, 13);
  VERIFY( i == 99 );
  std::fill_n(&i, -1, 13);
  VERIFY( i == 99 );
  std::fill_n(&i, -100, 13);
  VERIFY( i == 99 );
}

struct X
{
  X() { }
  X(const X&) { throw 1; }
  X& operator=(const X&) { throw 1u; }
};

void
test02()
{
  X x;
  std::fill_n(&x, 0, x);
  std::fill_n(&x, -1, x);
  std::fill_n(&x, -100, x);
}

int
main()
{
  test01();
  test02();
}

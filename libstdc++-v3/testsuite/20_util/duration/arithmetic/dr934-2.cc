// { dg-do run { target c++11 } }

// Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

#include <chrono>
#include <testsuite_hooks.h>

// DR 934.
void
test01()
{
  using namespace std::chrono;

  const duration<int> d0(17);
  duration<int> d3(d0);
  d3 %= 5;
  VERIFY( d3.count() == 2 );

  const duration<int> d4(7);
  duration<int> d5(d0);
  d5 %= d4;
  VERIFY( d5.count() == 3 );

  const duration<int> d6 = d0 % 6;
  VERIFY( d6.count() == 5 );

  const duration<int> d7(11);
  const duration<int> d8 = d0 % d7;
  VERIFY( d8.count() == 6 );
}

int
main()
{
  test01();
  return 0;
}

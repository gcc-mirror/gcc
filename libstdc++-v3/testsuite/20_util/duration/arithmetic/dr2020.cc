// { dg-do run { target c++11 } }

// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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

// 20.11.5 Class template duration [time.duration]

#include <chrono>
#include <testsuite_hooks.h>

// DR 2020
void test01()
{
  using namespace std::chrono;

  constexpr duration<int> d0(12);
  constexpr duration<int> d1(3);
  constexpr int i = 5;

  constexpr auto d2 = d0 + d1;
  VERIFY( d2.count() == 15 );

  constexpr auto d3 = d0 - d1;
  VERIFY( d3.count() == 9 );

  constexpr auto d4 = d0 * i;
  VERIFY( d4.count() == 60 );

  constexpr auto d5 = i * d0;
  VERIFY( d5.count() == 60 );

  constexpr auto d6 = d0 % i;
  VERIFY( d6.count() == 2 );

  constexpr auto j = d0 % d1;
  VERIFY( j.count() == 0 );

  constexpr auto d7 = d0 / i;
  VERIFY( d7.count() == 2 );

  constexpr auto k = d0 / d1;
  VERIFY( k == 4 );
}

int
main()
{
  test01();
  return 0;
}

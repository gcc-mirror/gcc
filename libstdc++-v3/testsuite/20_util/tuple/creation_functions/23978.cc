// { dg-do run { target c++11 } }
//
// Copyright (C) 2007-2016 Free Software Foundation, Inc.
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

// Tuple

#include <tuple>
#include <utility>
#include <testsuite_hooks.h>

using namespace std;

// libstdc++/23978
void test01()
{
  bool test __attribute__((unused)) = true;

  pair<int, int> p(1, 2);
  int x = 0;
  int y = 0;
  tie(x, y) = p;
  VERIFY( x == 1 && y == 2 );
}

int
main()
{
  test01();
  return 0;
}

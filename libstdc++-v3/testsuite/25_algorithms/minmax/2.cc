// { dg-do run { target c++11 } }

// 2008-09-16  Chris Fairles  <chris.fairles@gmail.com>

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  std::pair<int, int> z = std::minmax({1, 2, 3});
  std::pair<int, int> w = std::minmax({4, 3, 5, 4});
  std::pair<int, int> y = std::minmax({4, 5, 3, 7, 3});
  VERIFY( z.first == 1 );
  VERIFY( z.second == 3 );
  VERIFY( w.first == 3 );
  VERIFY( w.second == 5 );
  VERIFY( y.first == 3 );
  VERIFY( y.second == 7 );

  std::pair<int, int> zc =
    std::minmax({1, 2, 3}, std::greater<int>());

  std::pair<int, int> wc =
    std::minmax({4, 3, 5, 4}, std::greater<int>());

  std::pair<int, int> yc =
    std::minmax({4, 5, 3, 7, 3}, std::greater<int>());

  VERIFY( zc.first == 3 );
  VERIFY( zc.second == 1 );
  VERIFY( wc.first == 5 );
  VERIFY( wc.second == 3 );
  VERIFY( yc.first == 7 );
  VERIFY( yc.second == 3 );
}

int main()
{
  test01();
  return 0;
}

// { dg-do run { target c++17 } }

// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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
  const int x = std::clamp(1, 2, 4);
  const int y = std::clamp(3, 2, 4);
  const int z = std::clamp(5, 2, 4);
  VERIFY( x == 2 );
  VERIFY( y == 3 );
  VERIFY( z == 4 );

  const int xc = std::clamp(1, 4, 2, std::greater<int>());
  const int yc = std::clamp(3, 4, 2, std::greater<int>());
  const int zc = std::clamp(5, 4, 2, std::greater<int>());
  VERIFY( xc == 2 );
  VERIFY( yc == 3 );
  VERIFY( zc == 4 );
}

int
main()
{
  test01();
  return 0;
}

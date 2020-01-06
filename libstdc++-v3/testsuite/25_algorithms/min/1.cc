// 2000-03-29 sss/bkoz

// Copyright (C) 2000-2020 Free Software Foundation, Inc.
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
  const int z = std::min(1, 2);
  const int w = std::min(4, 3);
  VERIFY( z == 1 );
  VERIFY( w == 3 );

  const int zc = std::min(1, 2, std::greater<int>());
  const int wc = std::min(4, 3, std::greater<int>());
  VERIFY( zc == 2 );
  VERIFY( wc == 4 );
}

int main()
{
  test01();
  return 0;
}

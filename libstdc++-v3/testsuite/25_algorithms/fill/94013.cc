// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

void
test01()
{
  volatile unsigned char a[2] = { 1, 2 };
  volatile unsigned char c = 3;

  std::fill(a, a+2, c);
  VERIFY( a[0] == 3 && a[1] == 3 );

#if __cplusplus > 201703L
  c = 4;
  std::ranges::fill(a, c);
  VERIFY( a[0] == 4 && a[1] == 4 );
  unsigned char c2 = 5;
  std::ranges::fill(a, c2);
#endif
}

int
main()
{
  test01();
}

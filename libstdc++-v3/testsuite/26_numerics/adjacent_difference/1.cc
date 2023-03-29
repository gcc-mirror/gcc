// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

// 26.4.4 [lib.adjacent.difference]

#include <algorithm>
#include <numeric>
#include <testsuite_hooks.h>

int A[] = {1, 4, 9, 16, 25, 36, 49, 64, 81, 100};
int B[] = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19};
const int N = sizeof(A) / sizeof(int);

void
test01()
{
  int D[N];

  std::adjacent_difference(A, A + N, D);
  VERIFY( std::equal(D, D + N, B) );
}

int
main()
{
  test01();
  return 0;
}

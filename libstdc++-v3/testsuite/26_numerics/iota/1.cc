// { dg-do run { target c++11 } }

// 2008-06-27  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2008-2017 Free Software Foundation, Inc.
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

#include <numeric>
#include <algorithm>
#include <testsuite_hooks.h>

int A[] = {0, 0, 0, 0, 0, 0, 0, 0, 0};
int B[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
int C[] = {-9, -8, -7, -6, -5, -4, -3, -2, -1};
const int N = sizeof(A) / sizeof(int);

void
test01()
{
  std::iota(A, A + N, 1);
  VERIFY( std::equal(A, A + N, B) );

  std::iota(A, A + N, -9);
  VERIFY( std::equal(A, A + N, C) );
}

int
main()
{
  test01();
  return 0;
}

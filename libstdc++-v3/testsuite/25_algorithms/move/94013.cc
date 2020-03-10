// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <algorithm>
#include <testsuite_hooks.h>

void
test01()
{
  volatile int i[2] = { 1, 2 };
  volatile int j[2] = { 0, 0 };
  int k[2] = { 0, 0 };

  std::move(i, i+2, j);
  VERIFY( j[0] == 1 && j[1] == 2 );
  std::move(i, i+2, k);
  VERIFY( k[0] == 1 && k[1] == 2 );
  std::move(k+1, k+2, i);
  VERIFY( i[0] == 2 );

  const volatile int* cj = j;
  std::move(cj, cj+2, i);
  VERIFY( i[0] == 1 && i[1] == 2 );
  std::move(cj+1, cj+2, k);
  VERIFY( k[0] == 2 );
  const int* ck = k;
  std::move(ck, ck+2, i);
  VERIFY( i[0] == 2 && i[1] == 2 );
}

void
test02()
{
#if __cplusplus > 201703L
  volatile int i[2] = { 1, 2 };
  volatile int j[2] = { 0, 0 };
  int k[2] = { 0, 0 };

  std::ranges::move(i, i+2, j);
  VERIFY( j[0] == 1 && j[1] == 2 );
  std::ranges::move(i, i+2, k);
  VERIFY( k[0] == 1 && k[1] == 2 );
  std::ranges::move(k+1, k+2, i);
  VERIFY( i[0] == 2 );

  const volatile int* cj = j;
  std::ranges::move(cj, cj+2, i);
  VERIFY( i[0] == 1 && i[1] == 2 );
  std::ranges::move(cj+1, cj+2, k);
  VERIFY( k[0] == 2 );
  const int* ck = k;
  std::ranges::move(ck, ck+2, i);
  VERIFY( i[0] == 2 && i[1] == 2 );
#endif
}

int
main()
{
  test01();
  test02();
}

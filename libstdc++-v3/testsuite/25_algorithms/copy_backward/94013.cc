// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
  volatile int i[2] = { 1, 2 };
  volatile int j[2] = { 0, 0 };
  int k[2] = { 0, 0 };

  std::copy_backward(i, i+2, j+2);
  VERIFY( j[0] == 1 && j[1] == 2 );
  std::copy_backward(i, i+2, k+2);
  VERIFY( k[0] == 1 && k[1] == 2 );
  std::copy_backward(k+1, k+2, i+1);
  VERIFY( i[0] == 2 );

  const volatile int* cj = j;
  std::copy_backward(cj, cj+2, i+2);
  VERIFY( i[0] == 1 && i[1] == 2 );
  std::copy_backward(cj+1, cj+2, k+1);
  VERIFY( k[0] == 2 );
  const int* ck = k;
  std::copy_backward(ck, ck+2, i+2);
  VERIFY( i[0] == 2 && i[1] == 2 );
}

void
test02()
{
#if __cplusplus > 201703L
  volatile int i[2] = { 1, 2 };
  volatile int j[2] = { 0, 0 };
  int k[2] = { 0, 0 };

  std::ranges::copy_backward(i, i+2, j+2);
  VERIFY( j[0] == 1 && j[1] == 2 );
  std::ranges::copy_backward(i, i+2, k+2);
  VERIFY( k[0] == 1 && k[1] == 2 );
  std::ranges::copy_backward(k+1, k+2, i+1);
  VERIFY( i[0] == 2 );

  const volatile int* cj = j;
  std::ranges::copy_backward(cj, cj+2, i+2);
  VERIFY( i[0] == 1 && i[1] == 2 );
  std::ranges::copy_backward(cj+1, cj+2, k+1);
  VERIFY( k[0] == 2 );
  const int* ck = k;
  std::ranges::copy_backward(ck, ck+2, i+2);
  VERIFY( i[0] == 2 && i[1] == 2 );
#endif
}

int
main()
{
  test01();
  test02();
}

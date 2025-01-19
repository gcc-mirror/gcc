// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

// 26.4.2 [lib.inner_product]

#include <numeric>
#include <testsuite_hooks.h>

int A1[] = {1, 2, 3, 4,  5,  6,  7,  8,  9, 10};
int A2[] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29};
const int NA = sizeof(A1) / sizeof(int);

bool B1[] = {false, true, true, false, true, false, true, true, false, true};
bool B2[] = {true, false, true, true, false, true, false, true, true, false};
const int NB = sizeof(B1) / sizeof(bool);

void
test01()
{
  int res = std::inner_product(A1, A1 + NA, A2, 31);
  VERIFY( res == 983 );
}

void
test02()
{
  int res = std::inner_product(B1, B1 + NB, B2, 100);
  VERIFY( res == 102 );
}

int
main()
{
  test01();
  test02();
  return 0;
}

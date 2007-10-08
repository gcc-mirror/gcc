// Copyright (C) 2001, 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 26.4.1 [lib.accumulate]

#include <numeric>
#include <testsuite_hooks.h>

int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
const int NA = sizeof(A) / sizeof(int);

void
test01()
{
  bool test __attribute__((unused)) = true;

  int res = std::accumulate(A, A + NA, 11);
  VERIFY( res == 66 );
}

bool B[] = {true, false, true, true, false, true, false, true, true, false};
const int NB = sizeof(B) / sizeof(bool);

void
test02()
{
  bool test __attribute__((unused)) = true;

  int res = std::accumulate(B, B + NB, 100);
  VERIFY( res == 106 );
}

int
main()
{
    test01();
    test02();
    return 0;
}

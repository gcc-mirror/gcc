// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2007 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 25.2.8 [lib.alg.unique] Unique

#undef _GLIBCXX_CONCEPT_CHECKS

#include <vector>
#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, forward_iterator_wrapper> Container;

void test01()
{
  bool test __attribute__((unused)) = true;

  int intarray1[] = {1, 4, 4, 6, 1, 2, 2, 3, 1, 6, 6, 6, 5, 7, 5, 4, 4};
  int intarray2[] = {1, 1, 1, 2, 2, 1, 1, 7, 6, 6, 7, 8, 8, 8, 8, 9, 9};

  const int N = sizeof(intarray1) / sizeof(int);

  rvalstruct T1[N];
  rvalstruct T2[N];
  
  std::copy(intarray1,intarray1 + N, T1);
  std::copy(intarray2,intarray2 + N, T2);
  
  const int A1[] = {1, 4, 6, 1, 2, 3, 1, 6, 5, 7, 5, 4};
  const int B1[] = {1, 2, 1, 7, 6, 7, 8, 9};

  Container con(T1, T1 + N);

  VERIFY(std::unique(con.begin(), con.end()).ptr - T1 == 12);
  for(int i = 0; i < 12; ++i)
    VERIFY(T1[i].val == A1[i]);

  Container con2(T2, T2 + N);
  VERIFY(std::unique(con2.begin(), con2.end()).ptr - T2 == 8);
  for(int i = 0; i < 8; ++i)
    VERIFY(T2[i].val == B1[i]);
}


int main()
{
  test01();
  return 0;
}

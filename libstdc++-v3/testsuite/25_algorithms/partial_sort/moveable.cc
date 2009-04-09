// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2007, 2009 Free Software Foundation, Inc.
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

// 25.3.1.3 [lib.partial.sort]

#undef _GLIBCXX_CONCEPT_CHECKS
#define _GLIBCXX_TESTSUITE_ALLOW_RVALREF_ALIASING

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::rvalstruct;
using std::partial_sort;

typedef test_container<rvalstruct, random_access_iterator_wrapper> Container;

void 
test1()
{
  int intarray[] = {6, 5, 4, 3, 2, 1, 0};
  rvalstruct array[7];
  std::copy(intarray, intarray + 7, array);
  Container con(array, array + 7);
  partial_sort(con.begin(), con.it(3), con.end());
  VERIFY(array[0].val == 0 && array[1].val == 1 && array[2].val == 2);
  for(int i = 0; i < 7; ++i)
    VERIFY(array[i].valid);
}

void 
test2()
{
  int intarray[] = {0, 6, 1, 5, 2, 4, 3};
  rvalstruct array[7];
  std::copy(intarray, intarray + 7, array);
  Container con(array,array + 7);
  partial_sort(con.begin(), con.it(3), con.end());
  VERIFY(array[0].val == 0 && array[1].val == 1 && array[2].val == 2);
  for(int i = 0; i < 7; ++i)
    VERIFY(array[i].valid);
}

int 
main()
{
  test1();
  test2();
}

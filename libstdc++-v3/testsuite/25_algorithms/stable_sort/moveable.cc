// { dg-options "-std=gnu++0x" }

// Copyright (C) 2009 Free Software Foundation, Inc.
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

// 25.3.1.2 [lib.stable.sort]

#undef _GLIBCXX_CONCEPT_CHECKS

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, random_access_iterator_wrapper> Container;

const int A[] = { 10, 20, 1, 11, 2, 12, 3, 13, 4, 14, 5, 15, 6, 16, 7, 
		  17, 8, 18, 9, 19 };
const int N = sizeof(A) / sizeof(int);

// 25.3.1.2 stable_sort()
void
test01()
{
  bool test __attribute__((unused)) = true;

  rvalstruct s1[N];
  std::copy(A, A + N, s1);
  Container con(s1, s1 + N);
  std::stable_sort(con.begin(), con.end());
  VERIFY( s1[0].valid );
  for(int i = 1; i < N; ++i)
    VERIFY( s1[i].val>s1[i-1].val && s1[i].valid );
}

int
main()
{
  test01();
  return 0;
}

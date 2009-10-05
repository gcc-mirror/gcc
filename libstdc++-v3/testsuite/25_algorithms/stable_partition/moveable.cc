// { dg-options "-std=gnu++0x" }

// Copyright (C) 2009 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without Pred the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 25.2.12 [lib.alg.partitions] Partitions.

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using __gnu_test::test_container;
using __gnu_test::random_access_iterator_wrapper;
using __gnu_test::rvalstruct;

typedef test_container<rvalstruct, random_access_iterator_wrapper> Container;

const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
const int B[] = {2, 4, 6, 8, 10, 12, 14, 16, 1, 3, 5, 7, 9, 11, 13, 15, 17};
const int N = sizeof(A) / sizeof(int);

struct Pred
{
  bool
  operator()(const rvalstruct& x) const
  { return (x.val % 2) == 0; }
};

// 25.2.12 stable_partition()
void
test01()
{
  bool test __attribute__((unused)) = true;

  rvalstruct s1[N];
  std::copy(A, A + N, s1);
  Container con(s1, s1 + N);

  std::stable_partition(con.begin(), con.end(), Pred());
  VERIFY( std::equal(s1, s1 + N, B) );
}

int
main()
{
  test01();
  return 0;
}

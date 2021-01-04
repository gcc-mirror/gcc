// { dg-do run { target c++11 } }

// Copyright (C) 2009-2021 Free Software Foundation, Inc.
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

// Check that starting with a true predicate works too. (PR52822)
const int A2[] = {2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
const int B2[] = {2, 4, 6, 8, 10, 12, 14, 16, 3, 5, 7, 9, 11, 13, 15, 17};
const int N2 = sizeof(A2) / sizeof(int);

struct Pred
{
  bool
  operator()(const rvalstruct& x) const
  { return (x.val % 2) == 0; }
};

// 25.2.12 stable_partition(), starting with a false predicate.
void
test01()
{
  rvalstruct s1[N];
  std::copy(A, A + N, s1);
  Container con(s1, s1 + N);

  std::stable_partition(con.begin(), con.end(), Pred());
  VERIFY( std::equal(s1, s1 + N, B) );
}

// 25.2.12 stable_partition(), starting with a true predicate.
void
test02()
{
  rvalstruct s1[N2];
  std::copy(A2, A2 + N2, s1);
  Container con(s1, s1 + N2);

  std::stable_partition(con.begin(), con.end(), Pred());
  VERIFY( std::equal(s1, s1 + N2, B2) );
}

int
main()
{
  test01();
  test02();
  return 0;
}

// Copyright (C) 2001-2019 Free Software Foundation, Inc.
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
#include <testsuite_new_operators.h>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

const int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
const int B[] = {2, 4, 6, 8, 10, 12, 14, 16, 1, 3, 5, 7, 9, 11, 13, 15, 17};
const int N = sizeof(A) / sizeof(int);

// Index of the middle element that should be returned by the algo.
const int M = 8;

struct Pred
{
    bool
    operator()(const int& x) const
    { return (x % 2) == 0; }
};

// 25.2.12 stable_partition()
void
test01()
{
  using std::stable_partition;
  using __gnu_test::test_container;
  using __gnu_test::forward_iterator_wrapper;

  int s1[N];
  std::copy(A, A + N, s1);

  test_container<int, forward_iterator_wrapper> c(s1, s1+N);
  forward_iterator_wrapper<int> expected = c.begin();
  std::advance(expected, M);
  VERIFY( stable_partition(c.begin(), c.end(), Pred()) == expected);
  VERIFY( std::equal(s1, s1 + N, B) );
}

int
main()
{
  test01();

  // stable_partition rely on an internal buffer if possible. Try to limit the
  // size of this buffer to see if algo is robust.

  // Limit to half of the necessary buffer.
  __gnu_test::set_new_limit(sizeof(A) / 2);
  test01();

  // Limit to just 1 element.
  __gnu_test::set_new_limit(sizeof(int));
  test01();

  // Limit to 0
  __gnu_test::set_new_limit(0);
  test01();

  return 0;
}

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <list>
#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;


namespace ranges = std::ranges;

struct X
{
  int i;
};

void
test01()
{
    {
      X x[6] = { {2}, {2}, {6}, {8}, {2}, {11} };
      const int y[5] = { {2}, {6}, {8}, {2}, {11} };
      test_container<X, forward_iterator_wrapper> cx(x);
      auto res = ranges::unique(cx, {}, &X::i);
      VERIFY( res.end() == cx.end() );
      VERIFY( ranges::equal(cx.begin(), res.begin(), y, y+5, {}, &X::i) );
    }

    {
      X x[6] = { {2}, {2}, {6}, {8}, {2}, {11} };
      const int y[5] = { {2}, {6}, {8}, {2}, {11} };
      test_range<X, forward_iterator_wrapper> rx(x);
      auto res = ranges::unique(rx, {}, &X::i);
      VERIFY( res.end() == rx.end() );
      VERIFY( ranges::equal(rx.begin(), res.begin(), y, y+5, {}, &X::i) );
    }
}

constexpr bool
test02()
{
  int x[2] = {2, 2};
  const int y[1] = {2};
  auto res = ranges::unique(x);
  return ranges::equal(x, res.begin(), y, y+1, ranges::equal_to{});
}

/* The following is adapted from 25_algorithms/unique/2.cc.  */

namespace two_dot_cc
{
  const int T1[] = {1, 4, 4, 6, 1, 2, 2, 3, 1, 6, 6, 6, 5, 7, 5, 4, 4};
  const int T2[] = {1, 1, 1, 2, 2, 1, 1, 7, 6, 6, 7, 8, 8, 8, 8, 9, 9};
  const int N = sizeof(T1) / sizeof(int);

  const int A1[] = {1, 4, 6, 1, 2, 3, 1, 6, 5, 7, 5, 4};
  const int A2[] = {1, 4, 4, 6, 6, 6, 6, 7};
  const int A3[] = {1, 1, 1};

  const int B1[] = {1, 2, 1, 7, 6, 7, 8, 9};
  const int B2[] = {1, 1, 1, 2, 2, 7, 7, 8, 8, 8, 8, 9, 9};
  const int B3[] = {9, 9, 8, 8, 8, 8, 7, 6, 6, 1, 1, 1, 1, 1};

  void test01()
  {
    using namespace std;

    list<int>::iterator pos;

    list<int> coll(T1, T1 + N);
    pos = ranges::unique(coll.begin(), coll.end()).begin();
    VERIFY( equal(coll.begin(), pos, A1) );

    list<int> coll2(T2, T2 + N);
    pos = ranges::unique(coll2.begin(), coll2.end()).begin();
    VERIFY( equal(coll2.begin(), pos, B1) );
  }

  void test02()
  {
    using namespace std;

    list<int>::iterator pos;

    list<int> coll(T1, T1 + N);
    pos = ranges::unique(coll.begin(), coll.end(), greater<int>()).begin();
    VERIFY( equal(coll.begin(), pos, A2) );

    list<int> coll2(T2, T2 + N);
    pos = ranges::unique(coll2.begin(), coll2.end(), greater<int>()).begin();
    VERIFY( equal(coll2.begin(), pos, B2) );
  }

  void test03()
  {
    using namespace std;

    list<int>::iterator pos;

    list<int> coll(T1, T1 + N);
    pos = ranges::unique(coll.begin(), coll.end(), less<int>()).begin();
    VERIFY( equal(coll.begin(), pos, A3) );

    list<int> coll2(T2, T2 + N);
    reverse(coll2.begin(), coll2.end());
    pos = ranges::unique(coll2.begin(), coll2.end(), less<int>()).begin();
    VERIFY( equal(coll2.begin(), pos, B3) );
  }
} // namespace two_dot_cc

int main()
{
  test01();
  static_assert(test02());

  two_dot_cc::test01();
  two_dot_cc::test02();
  two_dot_cc::test03();

  return 0;
}

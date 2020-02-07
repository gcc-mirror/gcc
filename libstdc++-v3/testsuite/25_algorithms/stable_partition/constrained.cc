// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
    {
      int x[] = {1,2,3,4,5,6,7,8,9,10};
      test_container<int, bidirectional_iterator_wrapper> cx(x);
      auto pred = [] (int a) { return a%2==0; };
      auto range = ranges::stable_partition(cx, pred);
      VERIFY( ranges::all_of(cx.begin(), range.begin(), pred) );
      VERIFY( ranges::none_of(range, pred) );
    }

    {
      int x[] = {1,2,3,4,5,6,7,8,9,10,11};
      test_range<int, bidirectional_iterator_wrapper> cx(x);
      auto pred = [] (int a) { return a%2==0; };
      auto range = ranges::stable_partition(cx, pred);
      VERIFY( ranges::all_of(cx.begin(), range.begin(), pred) );
      VERIFY( ranges::none_of(range, pred) );
    }
}

void
test02()
{
  for (int k = 1; k <= 10; k++)
    {
      int x[] = {1,2,3,4,5,6,7,8,9,10};
      auto pred = [&] (int a) { return a >= k; };
      auto proj = [] (int a) { return a-1; };
      auto range = ranges::stable_partition(x, x+10, pred, proj);
      VERIFY( ranges::all_of(x, range.begin(), pred, proj) );
      VERIFY( ranges::none_of(range, pred, proj) );

      int y[] = {0,1,2,3,4,5,6,7,8,9};
      ranges::rotate(y, y+k);
      VERIFY( ranges::equal(x, y, {}, proj) );
    }
}

int
main()
{
  test01();
  test02();
}

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do run { target c++11 } }
// { dg-skip-if "" { *-*-* } { "-D_GLIBCXX_PARALLEL" } }

#include <algorithm>
#include <functional>
#include <testsuite_iterators.h>
#include <testsuite_hooks.h>

using __gnu_test::test_container;
using __gnu_test::forward_iterator_wrapper;

struct X
{
  int val;

  bool odd() const { return val % 2; }

  // Partitioned so that all odd values come before even values:
  bool operator<(const X& x) const { return this->odd() && !x.odd(); }
};

void
test01()
{
  // Test with range that is partitioned, but not sorted.
  X seq[] = { 1, 3, 5, 7, 1, 6, 4, 2 };
  test_container<X, forward_iterator_wrapper> c(seq);

  auto part1 = std::equal_range(c.begin(), c.end(), X{2});
  VERIFY( part1.first != c.end() && part1.second == c.end() );
  VERIFY( part1.first->val == 6 );
  auto part2 = std::equal_range(c.begin(), c.end(), X{2}, std::less<X>{});
  VERIFY( part2.first != c.end() && part1.second == c.end() );
  VERIFY( part2.first->val == 6 );

  auto part3 = std::equal_range(c.begin(), c.end(), X{9});
  VERIFY( part3.first == c.begin() && part3.second != c.end() );
  VERIFY( part3.second->val == 6 );
  auto part4 = std::equal_range(c.begin(), c.end(), X{9}, std::less<X>{});
  VERIFY( part4.first == c.begin() && part4.second != c.end() );
  VERIFY( part4.second->val == 6 );
}

int
main()
{
  test01();
}

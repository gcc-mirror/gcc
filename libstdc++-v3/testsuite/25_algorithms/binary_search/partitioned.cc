// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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
// { dg-skip-if "" { *-*-* } { "-D_GLIBCXX_PROFILE" } }

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
  X seq[] = { 1, 3, 5, 7, 1, 6, 4 };
  test_container<X, forward_iterator_wrapper> c(seq);

  auto b1 = std::binary_search(c.begin(), c.end(), X{2});
  VERIFY( b1 );
  auto b2 = std::binary_search(c.begin(), c.end(), X{2}, std::less<X>{});
  VERIFY( b2 );

  auto b3 = std::binary_search(c.begin(), c.end(), X{9});
  VERIFY( b3 );
  auto b4 = std::binary_search(c.begin(), c.end(), X{9}, std::less<X>{});
  VERIFY( b4 );

  auto b5 = std::binary_search(seq, seq+5, X{2});
  VERIFY( !b5 );
  auto b6 = std::binary_search(seq, seq+5, X{2}, std::less<X>{});
  VERIFY( !b6 );
}

int
main()
{
  test01();
}

// Copyright (C) 2016 Free Software Foundation, Inc.
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
  bool test __attribute((unused)) = true;

  // Test with range that is partitioned, but not sorted.
  X seq[] = { 1, 3, 5, 7, 1, 6, 4, 2 };
  test_container<X, forward_iterator_wrapper> c(seq);

  auto part1 = std::upper_bound(c.begin(), c.end(), X{2});
  VERIFY( part1 == c.end() );
  auto part2 = std::upper_bound(c.begin(), c.end(), X{2}, std::less<X>{});
  VERIFY( part2 == c.end() );

  auto part3 = std::upper_bound(c.begin(), c.end(), X{9});
  VERIFY( part3 != c.end() );
  VERIFY( part3->val == 6 );
  auto part4 = std::upper_bound(c.begin(), c.end(), X{9}, std::less<X>{});
  VERIFY( part3 != c.end() );
  VERIFY( part4->val == 6 );
}

struct Y
{
  double val;

  // Not irreflexive, so not a strict weak order.
  bool operator<(const Y& y) const { return val < (int)y.val; }
};

void
test02()
{
  bool test __attribute((unused)) = true;

  // Test that Debug Mode checks don't fire (libstdc++/71545)

  Y seq[] = { -0.1, 1.2, 5.0, 5.2, 5.1, 5.9, 5.5, 6.0 };
  test_container<Y, forward_iterator_wrapper> c(seq);

  auto part1 = std::upper_bound(c.begin(), c.end(), Y{5.5});
  VERIFY( part1 != c.end() );
  VERIFY( part1->val == 6.0 );
  auto part2 = std::upper_bound(c.begin(), c.end(), Y{5.5}, std::less<Y>{});
  VERIFY( part2 != c.end() );
  VERIFY( part2->val == 6.0 );

  auto part3 = std::upper_bound(c.begin(), c.end(), Y{1.0});
  VERIFY( part3 != c.end() );
  VERIFY( part3->val == 5.0 );
  auto part4 = std::upper_bound(c.begin(), c.end(), Y{1.0}, std::less<Y>{});
  VERIFY( part4 != c.end() );
  VERIFY( part4->val == 5.0 );
}

int
main()
{
  test01();
  test02();
}

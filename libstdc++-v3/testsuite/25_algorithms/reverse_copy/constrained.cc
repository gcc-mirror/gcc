// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_range;
using __gnu_test::test_container;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = { 1, 2, 3, 4 };
  int w[4];
  test_container<int, bidirectional_iterator_wrapper> cx(x), cw(w);
  const int y[] = { 4, 3, 2, 1 };

  auto [in,out] = ranges::reverse_copy(cx, cw.begin());
  VERIFY( in == ranges::end(cx) && out == cw.end() );
  VERIFY( ranges::equal(cw, y) );
}

void
test02()
{
  int x[] = { 1, 2, 3, 4, 5 };
  int w[5];
  test_range<int, bidirectional_iterator_wrapper> rx(x), rw(w);
  const int y[] = { 5, 4, 3, 2, 1 };

  auto [in,out] = ranges::reverse_copy(rx, ranges::begin(rw));
  VERIFY( in == ranges::end(rx) && out == ranges::end(rw) );
  VERIFY( ranges::equal(rw, y) );
}

constexpr bool
test03()
{
  const int x[] = { 1, 2, 3 };
  int w[2];
  const int y[] = { 2, 1 };
  ranges::reverse_copy(x, x+2, w);
  return ranges::equal(w, y);
}

int
main()
{
  test01();
  test02();

  static_assert(test03());
}

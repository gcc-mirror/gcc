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

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  const int c[6] = { 1, 2, 3, 4, 5, 6 };

    {
      int x[6] = { 7, 8, 9, 4, 5, 6 };
      int a = 1;
      VERIFY( ranges::generate_n(x, 3, [&] { return a++; }) == x+3 );
      VERIFY( ranges::equal(x, c) );
    }

    {
      int x[6] = { 7, 8, 9, 4, 5, 6 };
      int a = 1;
      test_container<int, forward_iterator_wrapper> cx(x);
      VERIFY( *ranges::generate_n(cx.begin(), 3, [&] { return a++; })
	       == 4 );
      VERIFY( ranges::equal(cx, c) );
    }

    {
      int x[6] = { 7, 8, 9, 4, 5, 6 };
      int a = 1;
      test_range<int, output_iterator_wrapper> rx(x);
      ranges::generate_n(ranges::begin(rx), 3, [&] { return a++; });
      VERIFY( ranges::equal(x, c) );
    }
}

constexpr bool
test02()
{
  bool ok = true;
  int c[6] = { 1, 2, 3, 4, 5, 6 };
  int x[6];
  int a = 1;
  ranges::generate_n(x, 6, [&] { return a++; });
  ok &= ranges::equal(x, c);
  ranges::generate_n(c, 0, [] { return -1; });
  ok &= ranges::equal(x, c);
  ranges::generate_n(c, -2, [] { return -1; });
  ok &= ranges::equal(x, c);
  return ok;
}

int
main()
{
  test01();
  static_assert(test02());
}

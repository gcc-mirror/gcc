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
      int x[6];
      int a = 1;
      VERIFY( ranges::generate(x, [&] { return a++; }) == x+6 );
      VERIFY( ranges::equal(x, c) );
    }

    {
      int x[6];
      int a = 1;
      test_container<int, forward_iterator_wrapper> cx(x);
      VERIFY( ranges::generate(cx, [&] { return a++; }) == cx.end() );
      VERIFY( ranges::equal(cx, c) );
    }

    {
      int x[6];
      int a = 1;
      test_range<int, output_iterator_wrapper> rx(x);
      VERIFY( ranges::generate(rx, [&] { return a++; }) == rx.end() );
      VERIFY( ranges::equal(x, c) );
    }
}

constexpr bool
test02()
{
  const int c[6] = { 1, 2, 3, 4, 5, 6 };
  int x[6];
  int a = 1;
  ranges::generate(x, [&] { return a++; });
  return ranges::equal(x, c);
}

int
main()
{
  test01();
  static_assert(test02());
}

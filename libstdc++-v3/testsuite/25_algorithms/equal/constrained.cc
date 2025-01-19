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
// { dg-require-effective-target hosted }

#include <algorithm>
#include <functional>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

struct X { int i; };

void
test01()
{
  int x[] = { {2}, {2}, {6}, {8}, {10}, {11}, {11} };
  int y[] = { {2}, {2}, {6}, {8}, {10}, {11}, {11} };
  X   z[] = { {2}, {6}, {8}, {10}, {2}, {2} };
  int w[] = { {1}, {1}, {1}, {1}, {1} };

  VERIFY( ranges::equal(w, w+4, w+1, w+5) );
  VERIFY( ranges::equal(w, w+5, w, w+5, ranges::greater{},
			[] (int a) { return a+1; }) );

  test_container<int, forward_iterator_wrapper> cx(x), cy(y);
  test_container<X, forward_iterator_wrapper> cz(z);
  VERIFY( ranges::equal(cx, cy) );
  VERIFY( !ranges::equal(cx, cy, {}, [] (int a) { return a+1; }) );
  VERIFY( !ranges::equal(cx, cz, {}, {}, &X::i) );

  test_range<int, input_iterator_wrapper> rx(x), ry(y);
  test_range<X, input_iterator_wrapper> rz(z);
  VERIFY( ranges::equal(rx, ry) );

  rx.bounds.first = x;
  ry.bounds.first = y;
  VERIFY( !ranges::equal(rx, ry, {}, {}, [] (int a) { return a+1; }) );

  rx.bounds.first = x;
  rz.bounds.first = z;
  VERIFY( !ranges::equal(rx, rz, {}, {}, &X::i) );
}

void
test02()
{
  static constexpr X x[] = { {2}, {2}, {6}, {8}, {10}, {11} };
  static constexpr X y[] = { {2}, {6}, {8}, {10}, {11}, {2} };
  static constexpr int z[] = { {2}, {6}, {8}, {10}, {2}, {2} };
  static constexpr int w[] = { {2}, {6}, {8}, {10}, {2}, {2} };

  static_assert(ranges::equal(z, w));
  static_assert(!ranges::equal(z, z+5, w+1, w+6));
  static_assert(!ranges::equal(z, z, {}, {}, [] (int a) { return a+1; }));
  static_assert(!ranges::equal(x, y, {}, &X::i, &X::i));
}

void
test03()
{
  std::vector<int> x = { {2}, {2}, {6}, {8}, {10}, {11} };
  std::vector<int> y = { {2}, {2}, {6}, {8}, {10}, {11} };
  std::vector<int> z = { {2}, {2}, {6}, {8}, {10}, {12} };
  VERIFY( ranges::equal(x, y) );
  VERIFY( !ranges::equal(x, z) );
}

int
main()
{
  test01();
  test02();
  test03();
}

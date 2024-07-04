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

#include <algorithm>
#include <iterator>
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
  int x[] = { {2}, {2}, {6}, {8}, {10}, {11} };
  int y[] = { {2}, {6}, {8}, {10}, {11}, {2} };
  int z[] = { {2}, {6}, {8}, {10}, {2}, {2} };

  VERIFY( ranges::is_permutation(x, x+6, y, y+6) );
  VERIFY( !ranges::is_permutation(x, x+6, y, y+5) );

  test_container<int, forward_iterator_wrapper> cx(x), cy(y), cz(z);
  test_range<int, forward_iterator_wrapper> rx(x), ry(y), rz(z);
  VERIFY( ranges::is_permutation(cx, ry) );
  VERIFY( !ranges::is_permutation(rx, cz) );
  VERIFY( ranges::is_permutation(rx, cy) );
  VERIFY( !ranges::is_permutation(cx, rz) );
}

void
test02()
{
  static constexpr X x[] = { {2}, {2}, {6}, {8}, {10}, {11} };
  static constexpr X y[] = { {2}, {6}, {8}, {10}, {11}, {2} };
  static constexpr int z[] = { {2}, {6}, {8}, {10}, {2}, {2} };
  static_assert(ranges::is_permutation(x, y, {}, &X::i, &X::i));
  static_assert(!ranges::is_permutation(x, z, {}, &X::i));
  static_assert(!ranges::is_permutation(z, y, {}, {}, &X::i));
}

void
test03()
{
  int x[] = { 1, 2, 3, 4 };
  int y[] = { 1, 2, 3, 3 };
  test_container<int, bidirectional_iterator_wrapper> cx(x);
  do
    do
      {
	VERIFY( ranges::is_permutation(cx, x) );
	VERIFY( !ranges::is_permutation(y, cx) );
      } while (std::next_permutation(y, y+4));
  while (std::next_permutation(std::begin(cx), std::end(cx)));
}

int
main()
{
  test01();
  test02();
  test03();
}

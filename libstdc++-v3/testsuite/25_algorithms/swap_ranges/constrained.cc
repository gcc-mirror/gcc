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
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

struct X
{
  int i;
  int moved = 0;

  constexpr X(int a) : i(a) { }

  constexpr X(const X&) = delete;
  constexpr X& operator=(const X&) = delete;

  constexpr X(X&& other)
  {
    *this = std::move(other);
  }

  constexpr X&
  operator=(X&& other)
  {
    other.moved++;
    i = other.i;
    return *this;
  }

  friend constexpr bool
  operator==(const X& a, const X& b)
  { return a.i == b.i; }
};

void
test01()
{
    {
      X x[7] = { 1, 2, 3, 4, 5, 6, 7 };
      X y[7] = { 2, 4, 3, 5, 8, 9, 1 };
      X z[7] = { 1, 2, 3, 4, 5, 6, 7 };
      X w[7] = { 2, 4, 3, 5, 8, 9, 1 };
      auto [x_iter, y_iter] = ranges::swap_ranges(x, y);
      VERIFY( ranges::equal(y, z) && x_iter == x+7 && y_iter == y+7 );
      VERIFY( ranges::equal(x, w) );
    }

    {
      int x[3] = { 1, 2, 3 };
      int y[4] = { 2, 4, 6, 0 };
      int z[3] = { 1, 2, 3 };
      int w[3] = { 2, 4, 6 };
      test_container<int, forward_iterator_wrapper> cx(x);
      test_container<int, forward_iterator_wrapper> cy(y);
      auto [x_iter, y_iter] = ranges::swap_ranges(cx, cy);
      VERIFY( ranges::equal(y, y+3, z, z+3) );
      VERIFY( x_iter.ptr == x+3 && y_iter.ptr == y+3 );
      VERIFY( y[3] == 0 );
      VERIFY( ranges::equal(x, w) );
    }

    {
      int x[3] = { 1, 2, 3 };
      int y[4] = { 2, 4, 6, 0 };
      int z[3] = { 1, 2, 3 };
      int w[3] = { 2, 4, 6 };
      test_range<int, input_iterator_wrapper> cx(x);
      test_range<int, input_iterator_wrapper> cy(y);
      auto [y_iter, x_iter] = ranges::swap_ranges(cy, cx);
      VERIFY( ranges::equal(y, y+3, z, z+3) );
      VERIFY( x_iter.ptr == x+3 && y_iter.ptr == y+3 );
      VERIFY( y[3] == 0 );
      VERIFY( ranges::equal(x, w) );
    }
}

constexpr bool
test02()
{
  bool ok = true;
  X x[7] = { 1, 2, 3, 4, 5, 6, 7 };
  X y[7] = { 2, 4, 3, 5, 8, 9, 1 };
  X z[7] = { 1, 2, 3, 4, 5, 6, 7 };
  X w[7] = { 2, 4, 3, 5, 8, 9, 1 };
  auto [x_iter, y_iter] = ranges::swap_ranges(x, y);
  ok &= ranges::equal(y, z) && x_iter == x+7 && y_iter == y+7;
  ok &= ranges::equal(x, w);
  return ok;
}

int
main()
{
  test01();
  static_assert(test02());
}



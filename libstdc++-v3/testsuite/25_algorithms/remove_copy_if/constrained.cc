// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

struct X
{
  int i;

  friend constexpr bool
  operator==(const X& a, const X& b)
  {
    return a.i == b.i;
  }
};

void
test01()
{
  auto is_negative_p = [] (int a) { return a < 0; };
  auto is_two_p = [] (int a) { return a == 2; };

    {
      const X x[6] = { {2}, {2}, {6}, {8}, {2}, {11} };
      X y[2];
      X z[2] = { {6}, {8} };
      auto [in, out] = ranges::remove_copy_if(x, x+5, y, is_two_p, &X::i);
      VERIFY( in == x+5 && out == y+2 );
      VERIFY( ranges::equal(y, z) );
    }

    {
      const X x[6] = { {2}, {2}, {6}, {8}, {10}, {11} };
      X y[5];
      X z[5] = { {2}, {2}, {6}, {8}, {10} };
      auto [in, out] = ranges::remove_copy_if(x, x+5, y, is_negative_p, &X::i);
      VERIFY( in == x+5 && out == y+5 );
      VERIFY( ranges::equal(x, x+5, y, y+5) && ranges::equal(y, z) );
    }

    {
      X x[6] = { {2}, {2}, {6}, {8}, {10}, {11} };
      X y[4];
      X z[4] = { {6}, {8}, {10}, {11} };
      test_container<X, forward_iterator_wrapper> cx(x), cy(y), cz(z);
      auto [in, out] = ranges::remove_copy_if(cx, cy.begin(), is_two_p, &X::i);
      VERIFY( in == cx.end() && out == cy.end() );
      VERIFY( ranges::equal(cy, cz) );
    }

    {
      X x[6] = { {2}, {2}, {6}, {8}, {10}, {11} };
      X y[4];
      const X z[4] = { {6}, {8}, {10}, {11} };
      test_range<X, input_iterator_wrapper> cx(x);
      test_range<X, output_iterator_wrapper> cy(y);
      auto [in, out] = ranges::remove_copy_if(cx, cy.begin(), is_two_p, &X::i);
      VERIFY( in == cx.end() && out == cy.end() );
      VERIFY( ranges::equal(y, z) );
    }
}

struct Y { int i; int j; };

constexpr bool
test02()
{
  bool ok = true;
  Y x[3] = { {3,2}, {2,4}, {3,6} };
  Y y[1];
  Y z[1] = { {2,4} };
  auto [in, out]
    = ranges::remove_copy_if(x, y, [] (int a) { return a%2 == 1; }, &Y::i);
  ok &= in == x+3;
  ok &= out == y+1;
  ok &= ranges::equal(y, z, {}, &Y::i, &Y::i);
  ok &= ranges::equal(y, z, {}, &Y::j, &Y::j);
  return ok;
}

int
main()
{
  test01();
  static_assert(test02());
}

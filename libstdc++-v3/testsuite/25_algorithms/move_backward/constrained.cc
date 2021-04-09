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
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

struct X
{
  int i;
  int moved = 0;

  constexpr X() : i(0) { }
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
      X y[7] = { 0, 0, 0, 0, 0, 0, 0 };
      X z[7] = { 1, 2, 3, 4, 5, 6, 7 };
      auto [in, out] = ranges::move_backward(x, y+7);
      VERIFY( ranges::equal(x, y) && in == x+7 && out == y );
      VERIFY( ranges::equal(x, z) );
    }

    {
      int x[3] = { 1, 2, 3 };
      char y[4] = { 0 };
      int z[3] = { 1, 2, 3 };
      test_container<int, bidirectional_iterator_wrapper> cx(x);
      test_container<char, bidirectional_iterator_wrapper> cy(y);
      auto [in, out] = ranges::move_backward(cx, cy.end());
      VERIFY( ranges::equal(x, x+3, y+1, y+4) && in.ptr == x+3 && out.ptr == y+1 );
      VERIFY( ranges::equal(x, z) );
    }

    {
      std::vector<char> x= {1,2,3};
      std::vector<int> y(3);
      const int z[3] = { 1, 2, 3 };
      auto [in, out] = ranges::move_backward(x, ranges::end(y));
      VERIFY( in == x.begin()+3 );
      VERIFY( out == y.begin() );
      VERIFY( ranges::equal(y, z) && ranges::equal(x, z) );
    }


    {
      std::vector<int> x = {1,2,3};
      std::vector<int> y(3);
      const int z[3] = { 1, 2, 3 };
      auto [in, out] = ranges::move_backward(x, ranges::end(y));
      VERIFY( in == x.begin()+3 );
      VERIFY( out == y.begin() );
      VERIFY( ranges::equal(y, z) && ranges::equal(x, z) );
    }

    {
      std::vector<int> x = {1,2,3};
      std::vector<int> y(3);
      const int z[3] = { 1, 2, 3 };
      auto [in,out] = ranges::move_backward(make_reverse_iterator(x.end()),
					    make_reverse_iterator(x.begin()),
					    make_reverse_iterator(y.begin()));
      VERIFY( in.base() == x.begin()+3 );
      VERIFY( out.base() == y.begin()+3 );
      VERIFY( ranges::equal(y, z) && ranges::equal(x, z) );
    }

    {
      std::vector<char> x = {1,2,3};
      std::vector<int> y(3);
      const int z[3] = { 1, 2, 3 };
      auto [in,out] = ranges::move_backward(make_reverse_iterator(x.end()),
					    make_reverse_iterator(x.begin()),
					    make_reverse_iterator(y.begin()));
      VERIFY( in.base() == x.begin()+3 );
      VERIFY( out.base() == y.begin()+3 );
      VERIFY( ranges::equal(y, z) && ranges::equal(x, z) );
    }
}

void
test02()
{
  X x[] = { {2}, {2}, {6}, {8}, {10} };
  X y[] = { {2}, {6}, {8}, {10}, {11}, {2} };
  const X z[] = { {2}, {2}, {6}, {8}, {10} };
  auto [in, out] = ranges::move_backward(x, ranges::end(y));
  VERIFY( ranges::equal(x, x+5, y+1, y+6) );
  VERIFY( in == x+5 );
  VERIFY( out == y+1 );
  VERIFY( y[0].i == 2 );
  VERIFY( ranges::equal(x, z) );
  VERIFY( ranges::count(x, 1, &X::moved) == 5 );
  VERIFY( ranges::count(y, 0, &X::moved) == 6 );
}

constexpr bool
test03()
{
  bool ok = true;
  X x[] = { {2}, {2}, {6}, {8}, {10} };
  X y[] = { {2}, {6}, {8}, {10}, {11}, {2} };
  const X z[] = { {2}, {2}, {6}, {8}, {10} };
  auto [in, out] = ranges::move_backward(x, ranges::end(y));
  ok &= ranges::equal(x, x+5, y+1, y+6);
  ok &= (in == x+5);
  ok &= (out == y+1);
  ok &= (y[0].i == 2);
  ok &= ranges::equal(x, z);
  ok &= ranges::count(x, 1, &X::moved) == 5;
  ok &= ranges::count(y, 0, &X::moved) == 6;
  return ok;
}

int
main()
{
  test01();
  test02();
  static_assert(test03());
}

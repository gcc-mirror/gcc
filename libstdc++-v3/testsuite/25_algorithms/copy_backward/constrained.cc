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
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
    {
      int x[7] = { 1, 2, 3, 4, 5, 6, 7 };
      int y[7] = { 0 };
      int z[7] = { 1, 2, 3, 4, 5, 6, 7 };
      auto [in, out] = ranges::copy_backward(x, ranges::end(y));
      VERIFY( ranges::equal(x, y) && in == x+7 && out == y);
      VERIFY( ranges::equal(x, z) );
    }

    {
      int x[3] = { 1, 2, 3 };
      char y[4] = { 0 };
      int z[3] = { 1, 2, 3 };
      test_container<int, bidirectional_iterator_wrapper> cx(x);
      test_container<char, bidirectional_iterator_wrapper> cy(y);
      auto [in, out] = ranges::copy_backward(cx, ranges::end(cy));
      VERIFY( ranges::equal(x, x+3, y+1, y+4) && in.ptr == x+3 && out.ptr == y+1 );
      VERIFY( ranges::equal(x, z) );
    }

    {
      std::vector<char> x = {1,2,3};
      std::vector<int> y(3);
      const int z[3] = { 1, 2, 3 };
      auto [in, out] = ranges::copy_backward(x, ranges::end(y));
      VERIFY( in == x.begin()+3 );
      VERIFY( out == y.begin() );
      VERIFY( ranges::equal(y, z) && ranges::equal(x, z) );
    }


    {
      std::vector<int> x = {1,2,3};
      std::vector<int> y(3);
      const int z[3] = { 1, 2, 3 };
      auto [in, out] = ranges::copy_backward(x, ranges::end(y));
      VERIFY( in == x.begin()+3 );
      VERIFY( out == y.begin() );
      VERIFY( ranges::equal(y, z) && ranges::equal(x, z) );
    }

    {
      std::vector<int> x = {1,2,3};
      std::vector<int> y(3);
      const int z[3] = { 1, 2, 3 };
      auto [in,out] = ranges::copy_backward(make_reverse_iterator(x.end()),
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
      auto [in,out] = ranges::copy_backward(make_reverse_iterator(x.end()),
					    make_reverse_iterator(x.begin()),
					    make_reverse_iterator(y.begin()));
      VERIFY( in.base() == x.begin()+3 );
      VERIFY( out.base() == y.begin()+3 );
      VERIFY( ranges::equal(y, z) && ranges::equal(x, z) );
    }
}

constexpr bool
test02()
{
  bool ok = true;
  int x[] = { {2}, {2}, {6}, {8}, {10} };
  int y[] = { {2}, {6}, {8}, {10}, {11}, {2} };
  const int z[] = { {2}, {2}, {6}, {8}, {10} };
  auto [in, out] = ranges::copy_backward(x, ranges::end(y));
  ok &= ranges::equal(x, x+5, y+1, y+6);
  ok &= (in == x+5);
  ok &= (out == y+1);
  ok &= (y[0] == 2);
  ok &= ranges::equal(x, z);
  return ok;
}

/*  move_iterators are always input_iterators and therefore do not model
 *  bidirectional_iterator, so I think the following tests are rightly invalid.

struct Y
{
  int i;
  int moved = 0;

  constexpr Y(int a) : i(a) { }

  constexpr Y(const Y&) = delete;
  constexpr Y& operator=(const Y&) = delete;

  constexpr Y(Y&& other)
  {
    *this = std::move(other);
  }

  constexpr Y&
  operator=(Y&& other)
  {
    other.moved++;
    i = other.i;
    return *this;
  }

  friend constexpr bool
  operator==(const Y& a, const Y& b)
  { return a.i == b.i; }
};

void
test02()
{
  Y x[7] = { 1, 2, 3, 4, 5, 6, 7 };
  Y y[7] = { 0, 0, 0, 0, 0, 0, 0 };
  Y z[7] = { 1, 2, 3, 4, 5, 6, 7 };
  test_range<Y, bidirectional_iterator_wrapper> rx(x);
  auto [in, out] = ranges::copy_backward(std::move_iterator{ranges::begin(rx)},
					 std::move_sentinel{ranges::end(rx)},
					 ranges::end(y));
  VERIFY( ranges::equal(x, y) && std::move(in).base().ptr == x+7 && out == y );
  VERIFY( ranges::equal(x, z) );
  for (const auto& v : x)
    VERIFY( v.moved == 1 );
  for (const auto& v : y)
    VERIFY( v.moved == 0 );
}

constexpr bool
test03()
{
  bool ok = true;
  Y x[7] = { 1, 2, 3, 4, 5, 6, 7 };
  Y y[7] = { 0, 0, 0, 0, 0, 0, 0 };
  Y z[7] = { 1, 2, 3, 4, 5, 6, 7 };
  auto [in, out] = ranges::copy_backward(std::move_iterator{ranges::begin(x)},
					 std::move_sentinel{ranges::end(x)},
					 ranges::end(y));
  ok &= ranges::equal(x, y);
  ok &= in.base() == x+7;
  ok &= out == y;
  ok &= ranges::equal(x, z);
  for (const auto& v : x)
    ok &= v.moved == 1;
  for (const auto& v : y)
    ok &= v.moved == 0;
  return ok;
}
*/

int
main()
{
  test01();
  static_assert(test02());
}

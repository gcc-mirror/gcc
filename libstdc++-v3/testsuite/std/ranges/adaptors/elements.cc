// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
#include <ranges>
#include <tuple>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = ranges::views;

void
test01()
{
  std::tuple<int, int> x[] = {{1,2},{3,4},{5,6}};
  auto v0 = x | views::elements<0>;
  VERIFY( ranges::equal(v0, (int[]){1,3,5}) );
  VERIFY( ranges::equal(v0, x | views::keys) );
  VERIFY( ranges::size(v0) == 3 );

  using R0 = decltype(v0);
  static_assert(ranges::random_access_range<R0>);
  static_assert(ranges::sized_range<R0>);

  auto v1 = x | views::reverse | views::elements<1> | views::reverse;
  VERIFY( ranges::equal(v1, (int[]){2,4,6}) );
  VERIFY( ranges::equal(v1, x | views::values) );
}

struct S
{
  friend bool
  operator==(std::input_iterator auto const& i, S)
  { return std::get<1>(*i) == 0; }
};

void
test02()
{
  // This verifies that P1994R1 (and LWG3406) is implemented.
  std::pair<std::pair<char, int>, long> x[]
    = {{{1,2},3l}, {{1,0},2l}, {{1,2},0l}};
  ranges::subrange r{ranges::begin(x), S{}};

  auto v = r | views::keys;
  VERIFY( ranges::equal(v, (std::pair<char, int>[]){{1,2},{1,0}}) );
  ranges::subrange v2{ranges::begin(v), S{}};
  VERIFY( ranges::equal(v2, (std::pair<char, int>[]){{1,2}}) );
}

struct X
{
  using Iter = __gnu_test::forward_iterator_wrapper<std::pair<int, X>>;

  friend auto operator-(Iter l, Iter r) { return l.ptr - r.ptr; }
};

void
test03()
{
  using ranges::next;
  using ranges::begin;

  // LWG 3483
  std::pair<int, X> x[3];
  __gnu_test::test_forward_range<std::pair<int, X>> r(x);
  auto v = views::elements<1>(r);
  auto b = begin(v);
  static_assert( !ranges::random_access_range<decltype(r)> );
  static_assert( std::sized_sentinel_for<decltype(b), decltype(b)> );
  VERIFY( (next(b, 1) - b) == 1 );
  const auto v_const = v;
  auto b_const = begin(v_const);
  VERIFY( (next(b_const, 2) - b_const) == 2 );
}

template<auto elements = views::elements<0>>
void
test04()
{
  // Verify SFINAE behavior.
  static_assert(!requires { elements(); });
  static_assert(!requires { elements(0, 0); });
  static_assert(!requires { elements(0); });
  static_assert(!requires { 0 | elements; });
}

void
test05()
{
  // LWG 3502
  std::vector<int> vec = {42};
  auto r1 = vec
    | views::transform([](auto c) { return std::make_tuple(c, c); })
    | views::keys;
  VERIFY( ranges::equal(r1, (int[]){42}) );

  std::tuple<int, int> a[] = {{1,2},{3,4}};
  auto r2 = a | views::keys;
  VERIFY( r2[0] == 1 && r2[1] == 3 );
}

void
test06()
{
  // PR libstdc++/100631
  auto r = views::iota(0)
    | views::filter([](int){ return true; })
    | views::take(42)
    | views::reverse
    | views::transform([](int) { return std::make_pair(42, "hello"); })
    | views::take(42)
    | views::keys;
  auto b = r.begin();
  auto e = r.end();
  VERIFY( e - b == 42 );
  VERIFY( b - e == -42 );
}

void
test07()
{
  // PR libstdc++/100631 comment #2
  auto r = views::iota(0)
    | views::transform([](int) { return std::make_pair(42, "hello"); })
    | views::keys;
  auto b = ranges::cbegin(r);
  auto e = ranges::end(r);
  b.base() == e.base();
  b == e;
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
}

// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#include <ranges>
#include <testsuite_hooks.h>

void
test01()
{
  int vals[5] = { };
  int* out = vals;
  for (int i : std::ranges::iota_view{1, 4})
    *out++ = i;
  VERIFY(out == vals + 3);
  VERIFY(vals[0] == 1);
  VERIFY(vals[1] == 2);
  VERIFY(vals[2] == 3);
  VERIFY(vals[3] == 0);
}

void
test02()
{
  auto v = std::ranges::views::iota(4);
  auto it = v.begin();
  VERIFY( *it == 4 );
  ++it;
  VERIFY( *it == 5 );
  it++;
  VERIFY( *it == 6 );
}

void
test03()
{
  auto v = std::ranges::views::iota(10, 15);
  auto it = v.begin();
  VERIFY( *it == 10 );
  it += 2;
  VERIFY( *it == 12 );
  it += 2;
  VERIFY( *it == 14 );
  ++it;
  VERIFY( it == v.end() );
}

void
test04()
{
  int x[] = {1,2,3};
  auto v = std::ranges::views::iota(std::counted_iterator(x, 3),
				    std::default_sentinel);
  auto it = v.begin();
  VERIFY( (*it).base() == x );
  ++it;
  VERIFY( (*it).base() == x+1 );
  ++it;
  VERIFY( (*it).base() == x+2 );
  ++it;
  VERIFY( it == v.end() );
}

// Verify we optimize away the 'bound' data member of an unbounded iota_view.
static_assert(sizeof(std::ranges::iota_view<char>) == 1);

void
test05()
{
  // PR libstdc++/100690
  int x[] = {42, 42, 42};
  auto r = std::views::iota(std::ranges::begin(x), std::ranges::cbegin(x) + 3);
  VERIFY( r.end() - r.begin() == 3 );
  VERIFY( r.begin() - r.end() == -3 );
}

void
test06()
{
  // Verify LWG 3523 changes.
  auto v1 = std::views::iota(0, 5);
  auto w1 = decltype(v1)(v1.begin(), v1.end());
  VERIFY( std::ranges::equal(v1, w1) );

  auto v2 = std::views::iota(0);
  auto w2 = decltype(v2)(v2.begin(), v2.end());
  static_assert(std::same_as<decltype(w2.end()), std::unreachable_sentinel_t>);
  VERIFY( *w2.begin() == 0 );

  auto v3 = std::views::iota(0, 5l);
  auto w3 = decltype(v3)(v3.begin(), v3.end());
  static_assert(!std::ranges::common_range<decltype(w3)>);
  VERIFY( std::ranges::equal(v3, w3) );
}

template<auto iota = std::views::iota>
void
test07()
{
  // Verify SFINAE behavior.
  static_assert(!requires { iota(nullptr); });
  static_assert(!requires { iota(nullptr, nullptr); });
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

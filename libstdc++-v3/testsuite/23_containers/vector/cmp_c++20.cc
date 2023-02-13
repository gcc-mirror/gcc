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
// { dg-do compile { target c++2a } }
// { dg-xfail-if "not supported" { debug_mode } }

#include <vector>
#include <testsuite_hooks.h>

constexpr bool
test01()
{
  std::vector<int> c1{ 1, 2, 3 }, c2{ 1, 2, 3, 4 }, c3{ 1, 2, 4 };
  VERIFY( c1 == c1 );
  VERIFY( std::is_eq(c1 <=> c1) );
  VERIFY( c1 < c2 );
  VERIFY( std::is_lt(c1 <=> c2) );
  VERIFY( c1 < c3 );
  VERIFY( std::is_lt(c1 <=> c3) );
  VERIFY( c2 < c3 );
  VERIFY( std::is_lt(c2 <=> c3) );

  static_assert( std::totally_ordered<std::vector<int>> );

  static_assert( std::three_way_comparable<std::vector<int>,
					   std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::vector<float>,
					     std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::vector<float>,
					     std::weak_ordering> );
  static_assert( std::three_way_comparable<std::vector<float>,
					   std::partial_ordering> );

  struct E
  {
    constexpr bool operator==(E) const { return true; }
  };
  static_assert( ! std::totally_ordered<std::vector<E>> );
  static_assert( ! std::three_way_comparable<E> );
  static_assert( ! std::three_way_comparable<std::vector<E>> );

  return true;
}

constexpr bool
test02()
{
  struct W
  {
    int value = 0;

    constexpr bool operator==(W rhs) const noexcept
    { return (value | 1) == (rhs.value | 1); }

    constexpr std::weak_ordering
    operator<=>(W rhs) const noexcept
    { return (value | 1) <=> (rhs.value | 1); }
  };

  static_assert( std::totally_ordered<std::vector<W>> );

  std::vector<W> c1{ {1}, {2}, {3} }, c2{ {0}, {3}, {3} };
  static_assert( std::same_as<decltype(c1 <=> c1), std::weak_ordering> );
  VERIFY( c1 == c2 );
  VERIFY( std::is_eq(c1 <=> c2) );

  return true;
}

constexpr bool
test03()
{
  struct P
  {
    int value = 0;

    constexpr bool operator==(P rhs) const noexcept
    {
      if (value < 0 || rhs.value < 0)
	return false;
      return value == rhs.value;
    }

    constexpr std::partial_ordering
    operator<=>(P rhs) const noexcept
    {
      if (value < 0 || rhs.value < 0)
	return std::partial_ordering::unordered;
      return value <=> rhs.value;
    }
  };

  static_assert( std::totally_ordered<std::vector<P>> );

  std::vector<P> c{ {1}, {2}, {-3} };
  static_assert( std::three_way_comparable<P> );
  static_assert( std::same_as<decltype(c <=> c), std::partial_ordering> );
  VERIFY( (c <=> c) == std::partial_ordering::unordered );

  return true;
}

constexpr bool
test04()
{
  struct L
  {
    int value = 0;

    constexpr bool operator<(L rhs) const noexcept { return value < rhs.value; }
  };

  static_assert( std::totally_ordered<std::vector<L>> );

  std::vector<L> c{ {1}, {2}, {3} }, d{ {1}, {2}, {3}, {4} };
  static_assert( std::same_as<decltype(c <=> c), std::weak_ordering> );
  VERIFY( std::is_lt(c <=> d) );

  return true;
}

constexpr bool
test05()
{
  // vector iterators are random access, so should support <=>

  std::vector<int> c{ 1, 2, 3 };
  VERIFY( c.begin() == c.cbegin() );
  VERIFY( std::is_eq(c.begin() <=> c.cbegin()) );

  VERIFY( c.begin() < c.end() );
  VERIFY( std::is_lt(c.begin() <=> c.end()) );

  VERIFY( c.begin() < c.cend() );
  VERIFY( std::is_lt(c.begin() <=> c.cend()) );

  VERIFY( c.crbegin() == c.rbegin() );
  VERIFY( std::is_eq(c.crbegin() <=> c.rbegin()) );

  VERIFY( c.rend() > c.rbegin() );
  VERIFY( std::is_gt(c.rend() <=> c.rbegin()) );

  static_assert( std::same_as<decltype(c.begin() <=> c.begin()),
			      std::strong_ordering> );

  return true;
}

static_assert( test01() );
static_assert( test02() );
static_assert( test03() );
static_assert( test04() );
static_assert( test05() );

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

#include <map>
#include <testsuite_hooks.h>

void
test01()
{
  std::map<int, int> c1{ {1,1}, {2,1}, {3,1} };
  std::map<int, int> c2{ {1,1}, {2,1}, {3,1}, {4,1} };
  std::map<int, int> c3{ {1,1}, {2,1}, {3,2} };
  VERIFY( c1 == c1 );
  VERIFY( std::is_eq(c1 <=> c1) );
  VERIFY( c1 < c2 );
  VERIFY( std::is_lt(c1 <=> c2) );
  VERIFY( c1 < c3 );
  VERIFY( std::is_lt(c1 <=> c3) );
  VERIFY( c2 < c3 );
  VERIFY( std::is_lt(c2 <=> c3) );

  static_assert( std::totally_ordered<std::map<int, int>> );

  static_assert( std::three_way_comparable<std::map<int, int>,
					   std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::map<int, float>,
					     std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::map<int, float>,
					     std::weak_ordering> );
  static_assert( std::three_way_comparable<std::map<int, float>,
					   std::partial_ordering> );

  struct E
  {
    bool operator==(E) { return true; }
  };
  static_assert( ! std::totally_ordered<std::map<int, E>> );
  static_assert( ! std::three_way_comparable<E> );
  static_assert( ! std::three_way_comparable<std::map<int, E>> );
}

void
test02()
{
  struct W
  {
    int value = 0;

    bool operator==(W rhs) const noexcept
    { return (value | 1) == (rhs.value | 1); }

    std::weak_ordering
    operator<=>(W rhs) const noexcept
    { return (value | 1) <=> (rhs.value | 1); }
  };

  static_assert( std::totally_ordered<std::map<int, W>> );

  using P = std::pair<const W, W>;
  std::map<W, W> c1{ P{1,1}, P{2,2}, P{3,3} }, c2{ P{1,0}, P{3,2}, P{3,3} };
  static_assert( std::same_as<decltype(c1 <=> c1), std::weak_ordering> );
  VERIFY( c1 == c2 );
  VERIFY( std::is_eq(c1 <=> c2) );
}

void
test04()
{
  struct L
  {
    int value = 0;

    bool operator<(L rhs) const noexcept { return value < rhs.value; }
  };

  static_assert( std::totally_ordered<std::map<int, L>> );

  using P = std::pair<const L, L>;
  std::map<L, L> c{ P{1,1}, P{2,2}, P{3,3} }, d{ P{1,1}, P{2,2}, P{3,4} };
  static_assert( std::same_as<decltype(c <=> c), std::weak_ordering> );
  VERIFY( std::is_lt(c <=> d) );
}

// Associative container iterators are not random access
static_assert( ! std::totally_ordered<std::map<int, int>::iterator> );
static_assert( ! std::three_way_comparable<std::map<int, int>::iterator> );

int
main()
{
  test01();
  test02();
  test04();
}

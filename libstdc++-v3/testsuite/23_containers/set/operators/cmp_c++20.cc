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

#include <set>
#include <testsuite_hooks.h>

void
test01()
{
  std::set<int> c1{ 1, 2, 3 }, c2{ 1, 2, 3, 4 }, c3{ 1, 2, 4 };
  VERIFY( c1 == c1 );
  VERIFY( std::is_eq(c1 <=> c1) );
  VERIFY( c1 < c2 );
  VERIFY( std::is_lt(c1 <=> c2) );
  VERIFY( c1 < c3 );
  VERIFY( std::is_lt(c1 <=> c3) );
  VERIFY( c2 < c3 );
  VERIFY( std::is_lt(c2 <=> c3) );

  static_assert( std::totally_ordered<std::set<int>> );

  static_assert( std::three_way_comparable<std::set<int>,
					   std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::set<float>,
					     std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::set<float>,
					     std::weak_ordering> );
  static_assert( std::three_way_comparable<std::set<float>,
					   std::partial_ordering> );

  struct E
  {
    bool operator==(E) { return true; }
  };
  struct Cmp
  {
    bool operator()(E, E) const { return false; }
  };
  static_assert( ! std::totally_ordered<std::set<E, Cmp>> );
  static_assert( ! std::three_way_comparable<E> );
  static_assert( ! std::three_way_comparable<std::set<E, Cmp>> );
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

  static_assert( std::totally_ordered<std::set<W>> );

  std::set<W> c1{ {1}, {2}, {3} }, c2{ {0}, {3}, {3} };
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

  static_assert( std::totally_ordered<std::set<L>> );

  std::set<L> c{ {1}, {2}, {3} }, d{ {1}, {2}, {3}, {4} };
  static_assert( std::same_as<decltype(c <=> c), std::weak_ordering> );
  VERIFY( std::is_lt(c <=> d) );
}

// Associative container iterators are not random access
static_assert( ! std::totally_ordered<std::set<int>::iterator> );
static_assert( ! std::three_way_comparable<std::set<int>::iterator> );

int
main()
{
  test01();
  test02();
  test04();
}

// Copyright (C) 2020 Free Software Foundation, Inc.
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

#include <list>
#include <testsuite_hooks.h>

void
test01()
{
  std::list<int> c1{ 1, 2, 3 }, c2{ 1, 2, 3, 4 }, c3{ 1, 2, 4 };
  VERIFY( c1 == c1 );
  VERIFY( std::is_eq(c1 <=> c1) );
  VERIFY( c1 < c2 );
  VERIFY( std::is_lt(c1 <=> c2) );
  VERIFY( c1 < c3 );
  VERIFY( std::is_lt(c1 <=> c3) );
  VERIFY( c2 < c3 );
  VERIFY( std::is_lt(c2 <=> c3) );

  static_assert( std::totally_ordered<std::list<int>> );

  static_assert( std::three_way_comparable<std::list<int>,
					   std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::list<float>,
					     std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::list<float>,
					     std::weak_ordering> );
  static_assert( std::three_way_comparable<std::list<float>,
					   std::partial_ordering> );

  struct E
  {
    bool operator==(E) { return true; }
  };
  static_assert( ! std::totally_ordered<std::list<E>> );
  static_assert( ! std::three_way_comparable<E> );
  static_assert( ! std::three_way_comparable<std::list<E>> );
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

  static_assert( std::totally_ordered<std::list<W>> );

  std::list<W> c1{ {1}, {2}, {3} }, c2{ {0}, {3}, {3} };
  static_assert( std::same_as<decltype(c1 <=> c1), std::weak_ordering> );
  VERIFY( c1 == c2 );
  VERIFY( std::is_eq(c1 <=> c2) );
}

void
test03()
{
  struct P
  {
    int value = 0;

    bool operator==(P rhs) const noexcept
    {
      if (value < 0 || rhs.value < 0)
	return false;
      return value == rhs.value;
    }

    std::partial_ordering
    operator<=>(P rhs) const noexcept
    {
      if (value < 0 || rhs.value < 0)
	return std::partial_ordering::unordered;
      return value <=> rhs.value;
    }
  };

  static_assert( std::totally_ordered<std::list<P>> );

  std::list<P> c{ {1}, {2}, {-3} };
  static_assert( std::three_way_comparable<P> );
  static_assert( std::same_as<decltype(c <=> c), std::partial_ordering> );
  VERIFY( (c <=> c) == std::partial_ordering::unordered );
}

void
test04()
{
  struct L
  {
    int value = 0;

    bool operator<(L rhs) const noexcept { return value < rhs.value; }
  };

  static_assert( std::totally_ordered<std::list<L>> );

  std::list<L> c{ {1}, {2}, {3} }, d{ {1}, {2}, {3}, {4} };
  static_assert( std::same_as<decltype(c <=> c), std::weak_ordering> );
  VERIFY( std::is_lt(c <=> d) );
}

static_assert( ! std::totally_ordered<std::list<int>::iterator> );
static_assert( ! std::three_way_comparable<std::list<int>::iterator> );

int
main()
{
  test01();
  test02();
  test03();
  test04();
}

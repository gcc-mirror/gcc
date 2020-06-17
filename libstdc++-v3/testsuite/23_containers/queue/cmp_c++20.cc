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

#include <queue>
#include <testsuite_hooks.h>

void
test01()
{
  std::queue<int> c1{ {1, 2, 3} }, c2{ {1, 2, 3, 4} }, c3{ {1, 2, 4} };
  VERIFY( c1 == c1 );
  VERIFY( std::is_eq(c1 <=> c1) );
  VERIFY( c1 < c2 );
  VERIFY( std::is_lt(c1 <=> c2) );
  VERIFY( c1 < c3 );
  VERIFY( std::is_lt(c1 <=> c3) );
  VERIFY( c2 < c3 );
  VERIFY( std::is_lt(c2 <=> c3) );

  static_assert( std::totally_ordered<std::queue<int>> );

  static_assert( std::three_way_comparable<std::queue<int>,
					   std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::queue<float>,
					     std::strong_ordering> );
  static_assert( ! std::three_way_comparable<std::queue<float>,
					     std::weak_ordering> );
  static_assert( std::three_way_comparable<std::queue<float>,
					   std::partial_ordering> );

  struct E
  {
    bool operator==(E) { return true; }
  };
  static_assert( ! std::three_way_comparable<E> );
  static_assert( ! std::three_way_comparable<std::queue<E>> );
}

int
main()
{
  test01();
}

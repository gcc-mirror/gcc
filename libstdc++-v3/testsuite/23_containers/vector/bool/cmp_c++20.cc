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

#include <vector>
#include <testsuite_hooks.h>

void
test01()
{
  std::vector<bool> c1{ 1, 0, 1 }, c2{ 1, 0, 1, 0 }, c3{ 1, 1, 1 };
  VERIFY( c1 == c1 );
  VERIFY( std::is_eq(c1 <=> c1) );
  VERIFY( c1 < c2 );
  VERIFY( std::is_lt(c1 <=> c2) );
  VERIFY( c1 < c3 );
  VERIFY( std::is_lt(c1 <=> c3) );
  VERIFY( c2 < c3 );
  VERIFY( std::is_lt(c2 <=> c3) );

  static_assert( std::totally_ordered<std::vector<bool>> );

  static_assert( std::three_way_comparable<std::vector<bool>,
					   std::strong_ordering> );
}

void
test05()
{
  // vector<bool> iterators are random access, so should support <=>

  std::vector<bool> c{ 1, 1, 1 };
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
}

int
main()
{
  test01();
  test05();
}

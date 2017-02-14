// Copyright (C) 2013-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do run { target c++11 } }

// Insert with hint, specific to this library implementation.

#include <iterator>
#include <unordered_map>
#include <testsuite_hooks.h>

void test01()
{
  typedef std::unordered_multimap<int, int> Map;
  typedef typename Map::value_type Pair;

  Map m;

  auto it1 = m.insert(Pair(0, 0));
  VERIFY( it1 != m.end() );
  VERIFY( it1->first == 0 );
  VERIFY( it1->second == 0 );

  auto it2 = m.insert(it1, Pair(0, 2));
  VERIFY( it2 != m.end() );
  VERIFY( it2 != it1 );
  VERIFY( it2->second == 2 );
  VERIFY( it2 == std::next(it1) );

  Pair p(0, 1);
  it2 = m.insert(it1, p);
  VERIFY( it2 == std::next(it1) );
}

struct hasher
{
  std::size_t operator()(int val) const
  { return val / 2; }
};

void test02()
{
  typedef std::unordered_multimap<int, int, hasher> Map;
  typedef typename Map::value_type Pair;

  Map m;

  auto it1 = m.insert(Pair(0, 0));
  auto it2 = m.insert(it1, Pair(1, 0));
  VERIFY( m.bucket(it1->first) == m.bucket(it2->first) );
  VERIFY( m.bucket_size(m.bucket(it1->first)) == 2 );

  auto it3 = m.insert(it2, Pair(2, 0));
  VERIFY( m.bucket(it3->first) != m.bucket(it2->first) );
  VERIFY( m.bucket_size(m.bucket(it3->first)) == 1 );

  auto it4 = m.insert(it1, Pair(0, 1));
  VERIFY( it4 == std::next(it1) );
  VERIFY( m.bucket_size(m.bucket(it1->first)) == 3 );
  VERIFY( m.bucket_size(m.bucket(it3->first)) == 1 );

  Pair p(1, 1);
  it4 = m.insert(it2, p);
  VERIFY( it4 == std::next(it2) );
  VERIFY( m.bucket_size(m.bucket(it1->first)) == 4 );
  auto range = m.equal_range(0);
  VERIFY( std::distance(range.first, range.second) == 2 );
  range = m.equal_range(1);
  VERIFY( std::distance(range.first, range.second) == 2 );
}

void test03()
{
  typedef std::unordered_multimap<int, int> Map;
  typedef typename Map::value_type Pair;

  Map m;

  auto it1 = m.insert(Pair(0, 0));
  VERIFY( it1 != m.end() );
  VERIFY( it1->first == 0 );
  VERIFY( it1->second == 0 );

  auto it2 = m.emplace_hint(it1, std::piecewise_construct,
				 std::make_tuple(0),
				 std::make_tuple(2));
  VERIFY( it2 != m.end() );
  VERIFY( it2 != it1 );
  VERIFY( it2->second == 2 );
  VERIFY( it2 == std::next(it1) );

  Pair p(0, 1);
  it2 = m.emplace_hint(it1, p);
  VERIFY( it2 == std::next(it1) );
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}

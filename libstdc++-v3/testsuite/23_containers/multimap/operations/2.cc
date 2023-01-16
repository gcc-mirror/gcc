// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <map>
#include <testsuite_hooks.h>

struct Cmp
{
  typedef void is_transparent;

  bool operator()(int i, long l) const { return i < l; }
  bool operator()(long l, int i) const { return l < i; }
  bool operator()(int i, int j) const { ++count; return i < j; }

  static int count;
};

int Cmp::count = 0;

using test_type = std::multimap<int, char, Cmp>;

test_type x{ { 1, '2' }, { 3, '4' }, { 3, '5' } };
const test_type& cx = x;

void
test01()
{
  Cmp::count = 0;

  auto it = x.find(1L);
  VERIFY( it != x.end() && it->second == '2' );
  it = x.find(2L);
  VERIFY( it == x.end() );

  auto cit = cx.find(3L);
  VERIFY( cit != cx.end() && cit->second == '4' );
  cit = cx.find(2L);
  VERIFY( cit == cx.end() );

  VERIFY( Cmp::count == 0 );

  static_assert(std::is_same<decltype(it), test_type::iterator>::value,
      "find returns iterator");
  static_assert(std::is_same<decltype(cit), test_type::const_iterator>::value,
      "const find returns const_iterator");
}

void
test02()
{
  Cmp::count = 0;

  auto n = x.count(1L);
  VERIFY( n == 1 );
  n = x.count(2L);
  VERIFY( n == 0 );

  auto cn = cx.count(3L);
  VERIFY( cn == 2 );
  cn = cx.count(2L);
  VERIFY( cn == 0 );

  VERIFY( Cmp::count == 0 );
}

void
test03()
{
  Cmp::count = 0;

  auto it = x.lower_bound(1L);
  VERIFY( it != x.end() && it->second == '2' );
  it = x.lower_bound(2L);
  VERIFY( it != x.end() && it->second == '4' );

  auto cit = cx.lower_bound(1L);
  VERIFY( cit != cx.end() && cit->second == '2' );
  cit = cx.lower_bound(2L);
  VERIFY( cit != cx.end() && cit->second == '4' );

  VERIFY( Cmp::count == 0 );

  static_assert(std::is_same<decltype(it), test_type::iterator>::value,
      "lower_bound returns iterator");
  static_assert(std::is_same<decltype(cit), test_type::const_iterator>::value,
      "const lower_bound returns const_iterator");
}

void
test04()
{
  Cmp::count = 0;

  auto it = x.upper_bound(1L);
  VERIFY( it != x.end() && it->second == '4' );
  it = x.upper_bound(3L);
  VERIFY( it == x.end() );

  auto cit = cx.upper_bound(1L);
  VERIFY( cit != cx.end() && cit->second == '4' );
  cit = cx.upper_bound(3L);
  VERIFY( cit == cx.end() );

  VERIFY( Cmp::count == 0 );

  static_assert(std::is_same<decltype(it), test_type::iterator>::value,
      "upper_bound returns iterator");
  static_assert(std::is_same<decltype(cit), test_type::const_iterator>::value,
      "const upper_bound returns const_iterator");
}

void
test05()
{
  Cmp::count = 0;

  auto it = x.equal_range(1L);
  VERIFY( it.first != it.second && it.first->second == '2' );
  it = x.equal_range(2L);
  VERIFY( it.first == it.second && it.first != x.end() );

  auto cit = cx.equal_range(3L);
  VERIFY( cit.first != cit.second && cit.first->second == '4' );
  VERIFY( std::distance(cit.first, cit.second) == 2 );
  cit = cx.equal_range(2L);
  VERIFY( cit.first == cit.second && cit.first != cx.end() );

  VERIFY( Cmp::count == 0 );

  using pair = std::pair<test_type::iterator, test_type::iterator>;
  static_assert(std::is_same<decltype(it), pair>::value,
      "equal_range returns pair<iterator, iterator>");
  using cpair = std::pair<test_type::const_iterator, test_type::const_iterator>;
  static_assert(std::is_same<decltype(cit), cpair>::value,
      "const equal_range returns pair<const_iterator, const_iterator>");
}


int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
}

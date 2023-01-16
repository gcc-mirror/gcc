// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++17 } }

#include <unordered_map>
#include <testsuite_hooks.h>

using test_type = std::unordered_multimap<int, int>;

void
test01()
{
  test_type c{ {1, 10}, { 1, 11 }, {2, 20}, { 2, 21}, {3, 30}, { 3, 31 } };
  test_type::node_type node;
  test_type::iterator pos;

  node = c.extract(0);
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 6 );

  pos = c.insert(std::move(node));
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 6 );
  VERIFY( pos == c.end() );

  node = c.extract(1);
  VERIFY( (bool)node );
  VERIFY( !node.empty() );
  VERIFY( c.size() == 5 );
  VERIFY( node.get_allocator() == c.get_allocator() );
  VERIFY( node.key() == 1 );
  int mapped = node.mapped();
  VERIFY( mapped == 10 || mapped == 11 );

  node.key() = 4;
  node.mapped() = 40;
  VERIFY( node.key() == 4 );
  VERIFY( node.mapped() == 40 );

  pos = c.insert(std::move(node));
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 6 );
  VERIFY( pos != c.end() );
  VERIFY( pos->first == 4 );
  VERIFY( pos->second == 40 );

  pos = c.insert(c.begin(), std::move(node));
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 6 );
  VERIFY( pos == c.end() );

  node = c.extract(1);
  mapped = node.mapped();
  pos = c.insert(c.begin(), std::move(node));
  VERIFY( !node );
  VERIFY( c.size() == 6 );
  VERIFY( pos != c.end() );
  VERIFY( pos->first == 1 );
  VERIFY( pos->second == mapped );

  test_type c2 = c;
  node = c2.extract(1);
  mapped = node.mapped();
  pos = c.insert(std::move(node));
  VERIFY( pos != c.end() );
  VERIFY( node.empty() );
  VERIFY( pos->first == 1 );
  VERIFY( pos->second == mapped );
}

void
test02()
{
  test_type c{ {1, 10}, { 1, 11 }, {2, 20}, { 2, 21}, {3, 30}, { 3, 31 } };
  test_type::node_type node;
  test_type::iterator pos;

  const int key = c.begin()->first;
  const int mapped = c.begin()->second;
  node = c.extract(c.begin());
  VERIFY( !node.empty() );
  VERIFY( c.size() == 5 );
  VERIFY( node.get_allocator() == c.get_allocator() );
  VERIFY( node.key() == key );
  VERIFY( node.mapped() == mapped );

  pos = c.insert(std::move(node));
  VERIFY( node.empty() );
  VERIFY( c.size() == 6 );
  VERIFY( pos != c.end() );
  VERIFY( pos->first == key );
  VERIFY( pos->second == mapped );
}

void
test03()
{
  struct hash : std::hash<int> { };
  struct equal : std::equal_to<int> { };
  using std::is_same_v;
  using compat_type1 = std::unordered_multimap<int, int, hash, equal>;
  static_assert( is_same_v<test_type::node_type, compat_type1::node_type> );
  using compat_type2 = std::unordered_map<int, int>;
  static_assert( is_same_v<test_type::node_type, compat_type2::node_type> );
  using compat_type3 = std::unordered_map<int, int, hash, equal>;
  static_assert( is_same_v<test_type::node_type, compat_type3::node_type> );
}

int
main()
{
  test01();
  test02();
  test03();
}

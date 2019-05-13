// Copyright (C) 2016-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }

#include <unordered_map>
#include <testsuite_hooks.h>

using test_type = std::unordered_map<int, int>;

void
test01()
{
  test_type c{ {1, 10}, {2, 20}, {3, 30} };
  test_type::node_type node;
  test_type::insert_return_type ins;
  test_type::iterator pos;

  node = c.extract(0);
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 3 );

  ins = c.insert(std::move(node));
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 3 );
  VERIFY( !ins.inserted );
  VERIFY( !ins.node );
  VERIFY( ins.position == c.end() );

  node = c.extract(1);
  VERIFY( (bool)node );
  VERIFY( !node.empty() );
  VERIFY( c.size() == 2 );
  VERIFY( node.get_allocator() == c.get_allocator() );
  VERIFY( node.key() == 1 );
  VERIFY( node.mapped() == 10 );

  node.key() = 4;
  node.mapped() = 40;
  VERIFY( node.key() == 4 );
  VERIFY( node.mapped() == 40 );

  ins = c.insert(std::move(node));
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 3 );
  VERIFY( ins.inserted );
  VERIFY( !ins.node );
  VERIFY( ins.position != c.end() );
  VERIFY( ins.position->first == 4 );
  VERIFY( ins.position->second == 40 );
  VERIFY( c.count(1) == 0 );
  VERIFY( c.count(4) == 1 );

  pos = c.insert(c.begin(), std::move(node));
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 3 );
  VERIFY( pos == c.end() );

  pos = c.insert(c.begin(), c.extract(2));
  VERIFY( c.size() == 3 );
  VERIFY( pos != c.end() );
  VERIFY( pos->first == 2 );
  VERIFY( pos->second == 20 );

  test_type c2 = c;
  node = c2.extract(3);
  ins = c.insert(std::move(node));
  VERIFY( node.empty() );
  VERIFY( ins.position != c.end() );
  VERIFY( !ins.inserted );
  VERIFY( !ins.node.empty() );
  VERIFY( ins.node.key() == 3 );
  VERIFY( ins.node.mapped() == 30 );
  auto hasher = c.hash_function();
  VERIFY( hasher(ins.position->first) == hasher(ins.node.key()) );
  auto eq = c.key_eq();
  VERIFY( eq(ins.position->first, ins.node.key()) );
}

void
test02()
{
  test_type c{ {1, 10}, {2, 20}, {3, 30} };
  test_type::node_type node;
  test_type::insert_return_type ins;

  const int key = c.begin()->first;
  node = c.extract(c.begin());
  VERIFY( (bool)node );
  VERIFY( !node.empty() );
  VERIFY( c.size() == 2 );
  VERIFY( node.get_allocator() == c.get_allocator() );
  VERIFY( node.key() == key );
  VERIFY( node.mapped() == (key * 10) );

  ins = c.insert(std::move(node));
  VERIFY( node.empty() );
  VERIFY( c.size() == 3 );
  VERIFY( ins.inserted );
  VERIFY( !ins.node );
  VERIFY( ins.position != c.end() );
  VERIFY( ins.position->first == key );
  VERIFY( ins.position->second == (key * 10) );
}

void
test03()
{
  struct hash : std::hash<int> { };
  struct equal : std::equal_to<int> { };
  using std::is_same_v;
  using compat_type1 = std::unordered_map<int, int, hash, equal>;
  static_assert( is_same_v<test_type::node_type, compat_type1::node_type> );
  using compat_type2 = std::unordered_multimap<int, int>;
  static_assert( is_same_v<test_type::node_type, compat_type2::node_type> );
  using compat_type3 = std::unordered_multimap<int, int, hash, equal>;
  static_assert( is_same_v<test_type::node_type, compat_type3::node_type> );
}

void
test04()
{
  // Check order of members in insert_return_type
  auto [pos, ins, node] = test_type::insert_return_type{};
  using std::is_same_v;
  static_assert( is_same_v<test_type::iterator, decltype(pos)> );
  static_assert( is_same_v<bool, decltype(ins)> );
  static_assert( is_same_v<test_type::node_type, decltype(node)> );
}

int
main()
{
  test01();
  test02();
  test03();
}

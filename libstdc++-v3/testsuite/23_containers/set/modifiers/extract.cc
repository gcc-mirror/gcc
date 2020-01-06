// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

#include <set>
#include <algorithm>
#include <testsuite_hooks.h>

using test_type = std::set<int>;

void
test01()
{
  test_type c{ 1, 2, 3 };
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
  VERIFY( node.value() == 1 );

  node.value() = 4;
  VERIFY( node.value() == 4 );

  ins = c.insert(std::move(node));
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 3 );
  VERIFY( ins.inserted );
  VERIFY( !ins.node );
  VERIFY( ins.position != c.end() );
  VERIFY( *ins.position == 4 );
  VERIFY( c.count(1) == 0 );
  VERIFY( c.count(4) == 1 );
  VERIFY( std::is_sorted(c.begin(), c.end()) );

  pos = c.insert(c.begin(), std::move(node));
  VERIFY( !node );
  VERIFY( node.empty() );
  VERIFY( c.size() == 3 );
  VERIFY( pos == c.end() );

  node = c.extract(2);
  pos = c.insert(c.begin(), std::move(node));
  VERIFY( c.size() == 3 );
  VERIFY( pos != c.end() );
  VERIFY( *pos == 2 );

  test_type c2 = c;
  node = c2.extract(3);
  ins = c.insert(std::move(node));
  VERIFY( node.empty() );
  VERIFY( ins.position != c.end() );
  VERIFY( !ins.inserted );
  VERIFY( !ins.node.empty() );
  VERIFY( ins.node.value() == 3 );
  VERIFY( *ins.position == ins.node.value() );
}

void
test02()
{
  test_type c{ 1, 2, 3 };
  test_type::node_type node;
  test_type::insert_return_type ins;

  node = c.extract(c.begin());
  VERIFY( (bool)node );
  VERIFY( !node.empty() );
  VERIFY( c.size() == 2 );
  VERIFY( node.get_allocator() == c.get_allocator() );
  VERIFY( node.value() == 1 );

  ins = c.insert(std::move(node));
  VERIFY( node.empty() );
  VERIFY( c.size() == 3 );
  VERIFY( ins.inserted );
  VERIFY( !ins.node );
  VERIFY( ins.position != c.end() );
  VERIFY( *ins.position == 1 );
}

void
test03()
{
  struct less : std::less<int> { };
  using std::is_same_v;
  using compat_type1 = std::set<int, less>;
  static_assert( is_same_v<test_type::node_type, compat_type1::node_type> );
  using compat_type2 = std::multiset<int>;
  static_assert( is_same_v<test_type::node_type, compat_type2::node_type> );
  using compat_type3 = std::multiset<int, less>;
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

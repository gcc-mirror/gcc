// 2006-11-25  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2024 Free Software Foundation, Inc.
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
//

#include <set>
#include <testsuite_hooks.h>

// A few tests for equal_range, in the occasion of libstdc++/29385.
void test01()
{
  using namespace std;

  set<int> s0;
  typedef set<int>::iterator iterator;
  typedef set<int>::const_iterator const_iterator;
  typedef pair<iterator, bool> insert_return_type;
  pair<iterator, iterator> pp0;

  pp0 = s0.equal_range(1);
  VERIFY( s0.count(1) == 0 );
  VERIFY( pp0.first == s0.end() );
  VERIFY( pp0.second == s0.end() );

  insert_return_type irt0 = s0.insert(1);
  insert_return_type irt1 = s0.insert(2);
  insert_return_type irt2 = s0.insert(3);
 
  pp0 = s0.equal_range(2);
  VERIFY( s0.count(2) == 1 );
  VERIFY( *pp0.first == 2 );
  VERIFY( *pp0.second == 3 );
  VERIFY( pp0.first == irt1.first );
  VERIFY( --pp0.first == irt0.first );
  VERIFY( pp0.second == irt2.first );

  s0.insert(3);
  insert_return_type irt3 = s0.insert(3);
  insert_return_type irt4 = s0.insert(4);

  pp0 = s0.equal_range(3);
  VERIFY( s0.count(3) == 1 );
  VERIFY( *pp0.first == 3 );
  VERIFY( *pp0.second == 4 );
  VERIFY( pp0.first == irt2.first );
  VERIFY( --pp0.first == irt1.first );
  VERIFY( pp0.second == irt4.first );

  insert_return_type irt5 = s0.insert(0);
  s0.insert(1);
  s0.insert(1);
  s0.insert(1);

  pp0 = s0.equal_range(1);
  VERIFY( s0.count(1) == 1 );
  VERIFY( *pp0.first == 1 );
  VERIFY( *pp0.second == 2 );
  VERIFY( pp0.first == irt0.first );
  VERIFY( --pp0.first == irt5.first );
  VERIFY( pp0.second == irt1.first );

  insert_return_type irt6 = s0.insert(5);
  s0.insert(5);
  s0.insert(5);

  pp0 = s0.equal_range(5);
  VERIFY( s0.count(5) == 1 );
  VERIFY( *pp0.first == 5 );
  VERIFY( pp0.first == irt6.first );
  VERIFY( --pp0.first == irt4.first );
  VERIFY( pp0.second == s0.end() );

  s0.insert(4);
  s0.insert(4);
  s0.insert(4);

  pp0 = s0.equal_range(4);
  VERIFY( s0.count(4) == 1 );
  VERIFY( *pp0.first == 4 );
  VERIFY( *pp0.second == 5 );
  VERIFY( pp0.first == irt4.first );
  VERIFY( --pp0.first == irt3.first );
  VERIFY( pp0.second == irt6.first );

  s0.insert(0);
  insert_return_type irt7 = s0.insert(0);
  s0.insert(1);

  pp0 = s0.equal_range(0);
  VERIFY( s0.count(0) == 1 );
  VERIFY( *pp0.first == 0 );
  VERIFY( *pp0.second == 1 );
  VERIFY( pp0.first == irt5.first );
  VERIFY( pp0.first == s0.begin() );
  VERIFY( pp0.second == irt0.first );

  const set<int>& s1 = s0;
  pair<const_iterator, const_iterator> pp1 = s1.equal_range(1);
  VERIFY( s1.count(1) == 1 );
  VERIFY( *pp1.first == 1 );
  VERIFY( *pp1.second == 2 );
  VERIFY( pp1.first == irt0.first );
  VERIFY( --pp1.first == irt7.first );
  VERIFY( pp1.second == irt1.first );
}

int
main()
{
  test01();
  return 0;
}

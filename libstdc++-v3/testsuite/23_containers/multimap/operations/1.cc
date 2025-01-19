// 2006-11-25  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2025 Free Software Foundation, Inc.
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

#include <map>
#include <testsuite_hooks.h>

// A few tests for equal_range, in the occasion of libstdc++/29385.
void test01()
{
  using namespace std;

  multimap<int, int> mm0;
  typedef multimap<int, int>::iterator iterator;
  typedef multimap<int, int>::const_iterator const_iterator;
  pair<iterator, iterator> pp0;
  typedef multimap<int, int>::value_type value_type;

  pp0 = mm0.equal_range(1);
  VERIFY( mm0.count(1) == 0 );
  VERIFY( pp0.first == mm0.end() );
  VERIFY( pp0.second == mm0.end() );

  iterator iter0 = mm0.insert(value_type(1, 1));
  iterator iter1 = mm0.insert(value_type(2, 2));
  iterator iter2 = mm0.insert(value_type(3, 3));
 
  pp0 = mm0.equal_range(2);
  VERIFY( mm0.count(2) == 1 );
  VERIFY( *pp0.first == value_type(2, 2) );
  VERIFY( *pp0.second == value_type(3, 3) );
  VERIFY( pp0.first == iter1 );
  VERIFY( --pp0.first == iter0 );
  VERIFY( pp0.second == iter2 );

  mm0.insert(value_type(3, 4));
  iterator iter3 = mm0.insert(value_type(3, 5));
  iterator iter4 = mm0.insert(value_type(4, 6));

  pp0 = mm0.equal_range(3);
  VERIFY( mm0.count(3) == 3 );
  VERIFY( *pp0.first == value_type(3, 3) );
  VERIFY( *pp0.second == value_type(4, 6) );
  VERIFY( pp0.first == iter2 );
  VERIFY( --pp0.first == iter1 );
  VERIFY( pp0.second == iter4 );

  iterator iter5 = mm0.insert(value_type(0, 7));
  mm0.insert(value_type(1, 8));
  mm0.insert(value_type(1, 9));
  mm0.insert(value_type(1, 10));

  pp0 = mm0.equal_range(1);
  VERIFY( mm0.count(1) == 4 );
  VERIFY( *pp0.first == value_type(1, 1) );
  VERIFY( *pp0.second == value_type(2, 2) );
  VERIFY( pp0.first == iter0 );
  VERIFY( --pp0.first == iter5 );
  VERIFY( pp0.second == iter1 );

  iterator iter6 = mm0.insert(value_type(5, 11));
  mm0.insert(value_type(5, 12));
  mm0.insert(value_type(5, 13));

  pp0 = mm0.equal_range(5);
  VERIFY( mm0.count(5) == 3 );
  VERIFY( *pp0.first == value_type(5, 11) );
  VERIFY( pp0.first == iter6 );
  VERIFY( --pp0.first == iter4 );
  VERIFY( pp0.second == mm0.end() );

  mm0.insert(value_type(4, 14));
  mm0.insert(value_type(4, 15));
  mm0.insert(value_type(4, 16));

  pp0 = mm0.equal_range(4);
  VERIFY( mm0.count(4) == 4 );
  VERIFY( *pp0.first == value_type(4, 6) );
  VERIFY( *pp0.second == value_type(5, 11) );
  VERIFY( pp0.first == iter4 );
  VERIFY( --pp0.first == iter3 );
  VERIFY( pp0.second == iter6 );

  mm0.insert(value_type(0, 17));
  iterator iter7 = mm0.insert(value_type(0, 18));
  mm0.insert(value_type(1, 19));

  pp0 = mm0.equal_range(0);
  VERIFY( mm0.count(0) == 3 );
  VERIFY( *pp0.first == value_type(0, 7) );
  VERIFY( *pp0.second == value_type(1, 1) );
  VERIFY( pp0.first == iter5 );
  VERIFY( pp0.first == mm0.begin() );
  VERIFY( pp0.second == iter0 );

  const multimap<int, int>& mm1 = mm0;
  pair<const_iterator, const_iterator> pp1 = mm1.equal_range(1);
  VERIFY( mm1.count(1) == 5 );
  VERIFY( *pp1.first == value_type(1, 1) );
  VERIFY( *pp1.second == value_type(2, 2) );
  VERIFY( pp1.first == iter0 );
  VERIFY( --pp1.first == iter7 );
  VERIFY( pp1.second == iter1 );
}

int
main()
{
  test01();
  return 0;
}

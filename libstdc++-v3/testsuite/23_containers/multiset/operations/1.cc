// 2006-11-25  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2018 Free Software Foundation, Inc.
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

  multiset<int> ms0;
  typedef multiset<int>::iterator iterator;
  typedef multiset<int>::const_iterator const_iterator;
  pair<iterator, iterator> pp0;

  pp0 = ms0.equal_range(1);
  VERIFY( ms0.count(1) == 0 );
  VERIFY( pp0.first == ms0.end() );
  VERIFY( pp0.second == ms0.end() );

  iterator iter0 = ms0.insert(1);
  iterator iter1 = ms0.insert(2);
  iterator iter2 = ms0.insert(3);
 
  pp0 = ms0.equal_range(2);
  VERIFY( ms0.count(2) == 1 );
  VERIFY( *pp0.first == 2 );
  VERIFY( *pp0.second == 3 );
  VERIFY( pp0.first == iter1 );
  VERIFY( --pp0.first == iter0 );
  VERIFY( pp0.second == iter2 );

  ms0.insert(3);
  iterator iter3 = ms0.insert(3);
  iterator iter4 = ms0.insert(4);

  pp0 = ms0.equal_range(3);
  VERIFY( ms0.count(3) == 3 );
  VERIFY( *pp0.first == 3 );
  VERIFY( *pp0.second == 4 );
  VERIFY( pp0.first == iter2 );
  VERIFY( --pp0.first == iter1 );
  VERIFY( pp0.second == iter4 );

  iterator iter5 = ms0.insert(0);
  ms0.insert(1);
  ms0.insert(1);
  ms0.insert(1);

  pp0 = ms0.equal_range(1);
  VERIFY( ms0.count(1) == 4 );
  VERIFY( *pp0.first == 1 );
  VERIFY( *pp0.second == 2 );
  VERIFY( pp0.first == iter0 );
  VERIFY( --pp0.first == iter5 );
  VERIFY( pp0.second == iter1 );

  iterator iter6 = ms0.insert(5);
  ms0.insert(5);
  ms0.insert(5);

  pp0 = ms0.equal_range(5);
  VERIFY( ms0.count(5) == 3 );
  VERIFY( *pp0.first == 5 );
  VERIFY( pp0.first == iter6 );
  VERIFY( --pp0.first == iter4 );
  VERIFY( pp0.second == ms0.end() );

  ms0.insert(4);
  ms0.insert(4);
  ms0.insert(4);

  pp0 = ms0.equal_range(4);
  VERIFY( ms0.count(4) == 4 );
  VERIFY( *pp0.first == 4 );
  VERIFY( *pp0.second == 5 );
  VERIFY( pp0.first == iter4 );
  VERIFY( --pp0.first == iter3 );
  VERIFY( pp0.second == iter6 );

  ms0.insert(0);
  iterator iter7 = ms0.insert(0);
  ms0.insert(1);

  pp0 = ms0.equal_range(0);
  VERIFY( ms0.count(0) == 3 );
  VERIFY( *pp0.first == 0 );
  VERIFY( *pp0.second == 1 );
  VERIFY( pp0.first == iter5 );
  VERIFY( pp0.first == ms0.begin() );
  VERIFY( pp0.second == iter0 );

  const multiset<int>& ms1 = ms0;
  pair<const_iterator, const_iterator> pp1 = ms1.equal_range(1);
  VERIFY( ms1.count(1) == 5 );
  VERIFY( *pp1.first == 1 );
  VERIFY( *pp1.second == 2 );
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

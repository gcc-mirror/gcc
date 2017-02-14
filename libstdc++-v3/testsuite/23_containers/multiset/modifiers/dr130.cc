// { dg-do run { target c++11 } }

// Copyright (C) 2009-2017 Free Software Foundation, Inc.
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


// NOTE: This makes use of the fact that we know how moveable
// is implemented on multiset (via swap). If the implementation changed
// this test may begin to fail.

#include <set>
#include <vector>
#include <testsuite_hooks.h>

using namespace std;

void
test01()
{
  using namespace std;

  multiset<int> ms0;
  typedef multiset<int>::iterator iterator;
  typedef multiset<int>::const_iterator const_iterator;
  typedef iterator insert_return_type;

  vector<insert_return_type> irt;
  for ( int i = 1; i <= 4; ++i )
    for (int j = 1; j <= i; ++j)
      irt.push_back( ms0.insert( i ) );

  iterator pos1 = ms0.erase(irt[1]);
  VERIFY( pos1 == irt[2] );

  iterator pos2 = ms0.erase(irt[2]);
  VERIFY( pos2 == irt[3] );

  iterator pos3 = ms0.erase(irt[9]);
  VERIFY( pos3 == ms0.end() );
}

void
test02()
{
  using namespace std;

  multiset<int> ms0;
  typedef multiset<int>::iterator iterator;
  typedef multiset<int>::const_iterator const_iterator;
  typedef iterator insert_return_type;

  vector<insert_return_type> irt;
  for ( int i = 1; i <= 4; ++i )
    for (int j = 1; j <= i; ++j)
      irt.push_back( ms0.insert( i ) );

  iterator pos1 = ms0.erase(irt[3], irt[6]);
  VERIFY( pos1 == irt[6] );

  iterator pos2 = ms0.erase(irt[6], ++irt[9]);
  VERIFY( pos2 == ms0.end() );
}

int
main()
{
  test01();
  test02();
}

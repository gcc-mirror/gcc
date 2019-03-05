// 2006-01-07  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2006-2019 Free Software Foundation, Inc.
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

// 23.3.4  Class template multiset

#include <set>
#include <testsuite_hooks.h>

// libstdc++/22102
void test01()
{
  typedef std::multiset<int>        Mset;
  typedef Mset::value_type          value_type;
  typedef Mset::iterator            iterator;
  
  Mset ms1;
  
  const iterator it1 = ms1.insert(value_type(0));
  const iterator it2 = ms1.insert(value_type(1));  
  const iterator it3 = ms1.insert(value_type(2));

  const value_type vt1(2);
  const iterator it4 = ms1.insert(it1, vt1);
  iterator it5 = it4;
  iterator it6 = it4;
  VERIFY( ms1.size() == 4 );
  VERIFY( *it4 == vt1 );
  VERIFY( ++it5 == it3 );
  VERIFY( --it6 == it2 );
  VERIFY( *it5 == *it3 );
  VERIFY( *it6 == *it2 );

  const value_type vt2(2);
  const iterator it7 = ms1.insert(ms1.begin(), vt2);
  iterator it8 = it7;
  iterator it9 = it7;
  VERIFY( ms1.size() == 5 );
  VERIFY( *it7 == vt2 );
  VERIFY( ++it8 == it4 );
  VERIFY( --it9 == it2 );
  VERIFY( *it8 == *it4 );
  VERIFY( *it9 == *it2 );

  const value_type vt3(2);
  const iterator it10 = ms1.insert(it1, vt3);
  iterator it11 = it10;
  iterator it12 = it10;
  VERIFY( ms1.size() == 6 );
  VERIFY( *it10 == vt3 );
  VERIFY( ++it11 == it7 );
  VERIFY( --it12 == it2 );
  VERIFY( *it11 == *it7 );
  VERIFY( *it12 == *it2 );

  const value_type vt4(0);
  const iterator it13 = ms1.insert(it10, vt4);
  iterator it14 = it13;
  iterator it15 = it13;
  VERIFY( ms1.size() == 7 );
  VERIFY( *it13 == vt4 );
  VERIFY( ++it14 == it2 );
  VERIFY( --it15 == it1 );
  VERIFY( *it14 == *it2 );
  VERIFY( *it15 == *it1 );

  const value_type vt5(1);
  const iterator it16 = ms1.insert(it13, vt5);
  iterator it17 = it16;
  iterator it18 = it16;
  VERIFY( ms1.size() == 8 );
  VERIFY( *it16 == vt5 );
  VERIFY( ++it17 == it2 );
  VERIFY( --it18 == it13 );
  VERIFY( *it17 == *it2 );
  VERIFY( *it18 == *it13 );

  const value_type vt6(0);
  const iterator it19 = ms1.insert(it1, vt6);
  iterator it20 = it19;
  VERIFY( ms1.size() == 9 );
  VERIFY( *it19 == vt6 );
  VERIFY( it19 == ms1.begin() );
  VERIFY( ++it20 == it1 );
  VERIFY( *it20 == *it1 );

  const value_type vt7(3);
  const iterator it21 = ms1.insert(it19, vt7);
  iterator it22 = it21;
  iterator it23 = it21;
  VERIFY( ms1.size() == 10 );
  VERIFY( *it21 == vt7 );
  VERIFY( ++it22 == ms1.end() );
  VERIFY( --it23 == it3 );
  VERIFY( *it23 == *it3 );

  const value_type vt8(2);
  const iterator it24 = ms1.insert(ms1.end(), vt8);
  iterator it25 = it24;
  iterator it26 = it24;
  VERIFY( ms1.size() == 11 );
  VERIFY( *it24 == vt8 );
  VERIFY( ++it25 == it21 );
  VERIFY( --it26 == it3 );
  VERIFY( *it25 == *it21 );
  VERIFY( *it26 == *it3 );   
    
  const value_type vt9(3);
  const iterator it27 = ms1.insert(it3, vt9);
  iterator it28 = it27;
  iterator it29 = it27;
  VERIFY( ms1.size() == 12 );
  VERIFY( *it27 == vt9 );
  VERIFY( ++it28 == it21 );
  VERIFY( --it29 == it24 );
  VERIFY( *it28 == *it21 );
  VERIFY( *it29 == *it24 ); 
}

int main()
{
  test01();
  return 0;
}

// 2007-02-22  Paolo Carlini  <pcarlini@suse.de> 
//
// Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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

// 6.3.4.4  Class template unordered_map

#include <tr1/unordered_map>
#include <string>
#include <testsuite_hooks.h>

// In the occasion of libstdc++/25896
void test01()
{
  bool test __attribute__((unused)) = true;
  
  typedef std::tr1::unordered_map<std::string, int> Map;
  typedef Map::iterator       iterator;
  typedef Map::const_iterator const_iterator;
  typedef Map::value_type     value_type;

  Map m1;

  m1.insert(value_type("because to why", 1));
  m1.insert(value_type("the stockholm syndrome", 2));
  m1.insert(value_type("a cereous night", 3));
  m1.insert(value_type("eeilo", 4));
  m1.insert(value_type("protean", 5));
  m1.insert(value_type("the way you are when", 6));
  m1.insert(value_type("tillsammans", 7));
  m1.insert(value_type("umbra/penumbra", 8));
  m1.insert(value_type("belonging (no longer mix)", 9));
  m1.insert(value_type("one line behind", 10));
  VERIFY( m1.size() == 10 );

  VERIFY( m1.erase("eeilo") == 1 );
  VERIFY( m1.size() == 9 );
  iterator it1 = m1.find("eeilo");
  VERIFY( it1 == m1.end() );

  VERIFY( m1.erase("tillsammans") == 1 );
  VERIFY( m1.size() == 8 );
  iterator it2 = m1.find("tillsammans");
  VERIFY( it2 == m1.end() );

  // Must work (see DR 526)
  iterator it3 = m1.find("belonging (no longer mix)");
  VERIFY( it3 != m1.end() );
  VERIFY( m1.erase(it3->first) == 1 );
  VERIFY( m1.size() == 7 );
  it3 = m1.find("belonging (no longer mix)");
  VERIFY( it3 == m1.end() );

  VERIFY( !m1.erase("abra") );
  VERIFY( m1.size() == 7 );

  VERIFY( !m1.erase("eeilo") );
  VERIFY( m1.size() == 7 );

  VERIFY( m1.erase("because to why") == 1 );
  VERIFY( m1.size() == 6 );
  iterator it4 = m1.find("because to why");
  VERIFY( it4 == m1.end() );

  iterator it5 = m1.find("umbra/penumbra");
  iterator it6 = m1.find("one line behind");
  VERIFY( it5 != m1.end() );
  VERIFY( it6 != m1.end() );

  VERIFY( m1.find("the stockholm syndrome") != m1.end() );
  VERIFY( m1.find("a cereous night") != m1.end() );
  VERIFY( m1.find("the way you are when") != m1.end() );
  VERIFY( m1.find("a cereous night") != m1.end() );

  VERIFY( m1.erase(it5->first) == 1 );
  VERIFY( m1.size() == 5 );
  it5 = m1.find("umbra/penumbra");
  VERIFY( it5 == m1.end() );

  VERIFY( m1.erase(it6->first) == 1 );
  VERIFY( m1.size() == 4 );
  it6 = m1.find("one line behind");
  VERIFY( it6 == m1.end() );

  iterator it7 = m1.begin();
  iterator it8 = it7;
  ++it8;
  iterator it9 = it8;
  ++it9;

  VERIFY( m1.erase(it8->first) == 1 );
  VERIFY( m1.size() == 3 );
  VERIFY( ++it7 == it9 );

  iterator it10 = it9;
  ++it10;
  iterator it11 = it10;

  VERIFY( m1.erase(it9->first) == 1 );
  VERIFY( m1.size() == 2 );
  VERIFY( ++it10 == m1.end() );

  VERIFY( m1.erase(m1.begin()) != m1.end() );  
  VERIFY( m1.size() == 1 );
  VERIFY( m1.begin() == it11 );

  VERIFY( m1.erase(m1.begin()->first) == 1 );  
  VERIFY( m1.size() == 0 );
  VERIFY( m1.begin() == m1.end() );
}

int main()
{
  test01();
  return 0;
}

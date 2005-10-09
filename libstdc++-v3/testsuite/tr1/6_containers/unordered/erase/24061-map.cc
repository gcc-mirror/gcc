// 2005-10-08  Paolo Carlini  <pcarlini@suse.de> 
//
// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 6.3.4.4  Class template unordered_map

#include <tr1/unordered_map>
#include <string>
#include <testsuite_hooks.h>

// libstdc++/24061
void test01()
{
  bool test __attribute__((unused)) = true;
  
  typedef std::tr1::unordered_map<std::string, int> Map;
  typedef Map::iterator       iterator;
  typedef Map::const_iterator const_iterator;
  typedef Map::value_type     value_type;
  
  Map m1;

  m1.insert(value_type("all the love in the world", 1));
  m1.insert(value_type("you know what you are?", 2));
  m1.insert(value_type("the collector", 3));
  m1.insert(value_type("the hand that feeds", 4));
  m1.insert(value_type("love is not enough", 5));
  m1.insert(value_type("every day is exactly the same", 6));
  m1.insert(value_type("with teeth", 7));
  m1.insert(value_type("only", 8));
  m1.insert(value_type("getting smaller", 9));
  m1.insert(value_type("sunspots", 10)); 
  VERIFY( m1.size() == 10 );

  iterator it1 = m1.begin();
  ++it1;
  iterator it2 = it1;
  ++it2;
  iterator it3 = m1.erase(it1);
  VERIFY( m1.size() == 9 );
  VERIFY( it3 == it2 );
  VERIFY( *it3 == *it2 );

  iterator it4 = m1.begin();
  ++it4;
  ++it4;
  ++it4;
  iterator it5 = it4;
  ++it5;
  ++it5;
  iterator it6 = m1.erase(it4, it5);
  VERIFY( m1.size() == 7 );
  VERIFY( it6 == it5 );
  VERIFY( *it6 == *it5 );

  const_iterator it7 = m1.begin();
  ++it7;
  ++it7;
  ++it7;
  const_iterator it8 = it7;
  ++it8;
  const_iterator it9 = m1.erase(it7);
  VERIFY( m1.size() == 6 );
  VERIFY( it9 == it8 );
  VERIFY( *it9 == *it8 );

  const_iterator it10 = m1.begin();
  ++it10;
  const_iterator it11 = it10;
  ++it11;
  ++it11;
  ++it11;
  ++it11;
  const_iterator it12 = m1.erase(it10, it11);
  VERIFY( m1.size() == 2 );
  VERIFY( it12 == it11 );
  VERIFY( *it12 == *it11 );
  VERIFY( ++it12 == m1.end() );

  iterator it13 = m1.erase(m1.begin(), m1.end());
  VERIFY( m1.size() == 0 );
  VERIFY( it13 == it12 );
  VERIFY( it13 == m1.begin() );
}
  
int main()
{
  test01();
  return 0;
}

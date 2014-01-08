// 2007-02-22  Paolo Carlini  <pcarlini@suse.de> 
//
// Copyright (C) 2007-2014 Free Software Foundation, Inc.
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

// 6.3.4.3  Class template unordered_set

#include <tr1/unordered_set>
#include <string>
#include <testsuite_hooks.h>

// In the occasion of libstdc++/25896
void test01()
{
  bool test __attribute__((unused)) = true;
  
  typedef std::tr1::unordered_set<std::string> Set;
  typedef Set::iterator       iterator;
  typedef Set::const_iterator const_iterator;

  Set s1;
  
  s1.insert("because to why");
  s1.insert("the stockholm syndrome");
  s1.insert("a cereous night");
  s1.insert("eeilo");
  s1.insert("protean");
  s1.insert("the way you are when");
  s1.insert("tillsammans");
  s1.insert("umbra/penumbra");
  s1.insert("belonging (no longer mix)");
  s1.insert("one line behind");
  VERIFY( s1.size() == 10 );

  VERIFY( s1.erase("eeilo") == 1 );
  VERIFY( s1.size() == 9 );
  iterator it1 = s1.find("eeilo");
  VERIFY( it1 == s1.end() );

  VERIFY( s1.erase("tillsammans") == 1 );
  VERIFY( s1.size() == 8 );
  iterator it2 = s1.find("tillsammans");
  VERIFY( it2 == s1.end() );

  // Must work (see DR 526)
  iterator it3 = s1.find("belonging (no longer mix)");
  VERIFY( it3 != s1.end() );
  VERIFY( s1.erase(*it3) == 1 );
  VERIFY( s1.size() == 7 );
  it3 = s1.find("belonging (no longer mix)");
  VERIFY( it3 == s1.end() );

  VERIFY( !s1.erase("abra") );
  VERIFY( s1.size() == 7 );

  VERIFY( !s1.erase("eeilo") );
  VERIFY( s1.size() == 7 );

  VERIFY( s1.erase("because to why") == 1 );
  VERIFY( s1.size() == 6 );
  iterator it4 = s1.find("because to why");
  VERIFY( it4 == s1.end() );

  iterator it5 = s1.find("umbra/penumbra");
  iterator it6 = s1.find("one line behind");
  VERIFY( it5 != s1.end() );
  VERIFY( it6 != s1.end() );

  VERIFY( s1.find("the stockholm syndrome") != s1.end() );
  VERIFY( s1.find("a cereous night") != s1.end() );
  VERIFY( s1.find("the way you are when") != s1.end() );
  VERIFY( s1.find("a cereous night") != s1.end() );

  VERIFY( s1.erase(*it5) == 1 );
  VERIFY( s1.size() == 5 );
  it5 = s1.find("umbra/penumbra");
  VERIFY( it5 == s1.end() );

  VERIFY( s1.erase(*it6) == 1 );
  VERIFY( s1.size() == 4 );
  it6 = s1.find("one line behind");
  VERIFY( it6 == s1.end() );

  iterator it7 = s1.begin();
  iterator it8 = it7;
  ++it8;
  iterator it9 = it8;
  ++it9;

  VERIFY( s1.erase(*it8) == 1 );
  VERIFY( s1.size() == 3 );
  VERIFY( ++it7 == it9 );

  iterator it10 = it9;
  ++it10;
  iterator it11 = it10;

  VERIFY( s1.erase(*it9) == 1 );
  VERIFY( s1.size() == 2 );
  VERIFY( ++it10 == s1.end() );

  VERIFY( s1.erase(s1.begin()) != s1.end() );  
  VERIFY( s1.size() == 1 );
  VERIFY( s1.begin() == it11 );

  VERIFY( s1.erase(*s1.begin()) == 1 );  
  VERIFY( s1.size() == 0 );
  VERIFY( s1.begin() == s1.end() );
}

int main()
{
  test01();
  return 0;
}

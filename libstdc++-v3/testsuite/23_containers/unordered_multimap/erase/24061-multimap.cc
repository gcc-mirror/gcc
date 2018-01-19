// { dg-do run { target c++11 } }

// 2010-02-10  Paolo Carlini  <paolo.carlini@oracle.com> 
//
// Copyright (C) 2010-2018 Free Software Foundation, Inc.
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

#include <unordered_map>
#include <string>
#include <testsuite_hooks.h>

// libstdc++/24061
void test01()
{
  typedef std::unordered_multimap<std::string, int> Mmap;
  typedef Mmap::iterator       iterator;
  typedef Mmap::const_iterator const_iterator;
  typedef Mmap::value_type     value_type;
  
  Mmap mm1;

  mm1.insert(value_type("all the love in the world", 1));
  mm1.insert(value_type("you know what you are?", 2));
  mm1.insert(value_type("the collector", 3));
  mm1.insert(value_type("the hand that feeds", 4));
  mm1.insert(value_type("love is not enough", 5));
  mm1.insert(value_type("every day is exactly the same", 6));
  mm1.insert(value_type("with teeth", 7));
  mm1.insert(value_type("only", 8));
  mm1.insert(value_type("getting smaller", 9));
  mm1.insert(value_type("sunspots", 10));

  mm1.insert(value_type("you know what you are?", 5));
  mm1.insert(value_type("the collector", 6));
  mm1.insert(value_type("the hand that feeds", 7));
  VERIFY( mm1.size() == 13 );

  iterator it1 = mm1.begin();
  ++it1;
  iterator it2 = it1;
  ++it2;
  iterator it3 = mm1.erase(it1);
  VERIFY( mm1.size() == 12 );
  VERIFY( it3 == it2 );
  VERIFY( *it3 == *it2 );

  iterator it4 = mm1.begin();
  ++it4;
  ++it4;
  ++it4;
  iterator it5 = it4;
  ++it5;
  ++it5;
  iterator it6 = mm1.erase(it4, it5);
  VERIFY( mm1.size() == 10 );
  VERIFY( it6 == it5 );
  VERIFY( *it6 == *it5 );

  const_iterator it7 = mm1.begin();
  ++it7;
  ++it7;
  ++it7;
  const_iterator it8 = it7;
  ++it8;
  const_iterator it9 = mm1.erase(it7);
  VERIFY( mm1.size() == 9 );
  VERIFY( it9 == it8 );
  VERIFY( *it9 == *it8 );

  const_iterator it10 = mm1.begin();
  ++it10;
  const_iterator it11 = it10;
  ++it11;
  ++it11;
  ++it11;
  ++it11;
  const_iterator it12 = mm1.erase(it10, it11);
  VERIFY( mm1.size() == 5 );
  VERIFY( it12 == it11 );
  VERIFY( *it12 == *it11 );

  iterator it13 = mm1.erase(mm1.begin(), mm1.end());
  VERIFY( mm1.size() == 0 );
  VERIFY( it13 == mm1.end() );
  VERIFY( it13 == mm1.begin() );
}
  
int main()
{
  test01();
  return 0;
}

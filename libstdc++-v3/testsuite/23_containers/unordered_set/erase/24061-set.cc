// { dg-options "-std=gnu++0x" }

// 2010-02-10  Paolo Carlini  <paolo.carlini@oracle.com> 
//
// Copyright (C) 2010 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <string>
#include <testsuite_hooks.h>

// libstdc++/24061
void test01()
{
  bool test __attribute__((unused)) = true;
  
  typedef std::unordered_set<std::string> Set;
  typedef Set::iterator       iterator;
  typedef Set::const_iterator const_iterator;

  Set s1;
  
  s1.insert("all the love in the world");
  s1.insert("you know what you are?");
  s1.insert("the collector");
  s1.insert("the hand that feeds");
  s1.insert("love is not enough");
  s1.insert("every day is exactly the same");
  s1.insert("with teeth");
  s1.insert("only");
  s1.insert("getting smaller");
  s1.insert("sunspots");
  VERIFY( s1.size() == 10 );

  iterator it1 = s1.begin();
  ++it1;
  iterator it2 = it1;
  ++it2;
  s1.erase(it1);
  VERIFY( s1.size() == 9 );

  iterator it4 = s1.begin();
  ++it4;
  ++it4;
  ++it4;
  iterator it5 = it4;
  ++it5;
  ++it5;
  s1.erase(it4, it5);
  VERIFY( s1.size() == 7 );

  const_iterator it7 = s1.begin();
  ++it7;
  ++it7;
  ++it7;
  const_iterator it8 = it7;
  ++it8;
  s1.erase(it7);
  VERIFY( s1.size() == 6 );

  const_iterator it10 = s1.begin();
  ++it10;
  const_iterator it11 = it10;
  ++it11;
  ++it11;
  ++it11;
  ++it11;
  s1.erase(it10, it11);
  VERIFY( s1.size() == 2 );

  s1.erase(s1.begin(), s1.end());
  VERIFY( s1.size() == 0 );
}
  
int main()
{
  test01();
  return 0;
}

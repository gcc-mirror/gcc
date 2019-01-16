// { dg-do run { target c++11 } }

// 2010-02-10  Paolo Carlini  <paolo.carlini@oracle.com> 
//
// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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
  typedef std::unordered_multiset<std::string> Mset;
  typedef Mset::iterator       iterator;
  typedef Mset::const_iterator const_iterator;

  Mset ms1;
  
  iterator it1 = ms1.insert(ms1.begin(), "all the love in the world");
  VERIFY( ms1.size() == 1 );
  VERIFY( *it1 == "all the love in the world" );
  
  const_iterator cit1(it1);
  const_iterator cit2 = ms1.insert(cit1, "you know what you are?");
  VERIFY( ms1.size() == 2 );
  VERIFY( cit2 != cit1 );
  VERIFY( *cit2 == "you know what you are?" );

  iterator it2 = ms1.insert(it1, "all the love in the world");
  VERIFY( ms1.size() == 3 );
  VERIFY( it2 != it1 );
  VERIFY( *it2 == "all the love in the world" );
}
  
int main()
{
  test01();
  return 0;
}

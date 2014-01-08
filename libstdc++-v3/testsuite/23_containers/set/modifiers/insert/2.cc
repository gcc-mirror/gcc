// { dg-options "-std=gnu++0x" }

// 2010-11-10  Paolo Carlini  <paolo.carlini@oracle.com> 
//
// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

#include <iterator>
#include <set>
#include <testsuite_hooks.h>
#include <testsuite_rvalref.h>

void test01()
{
  bool test __attribute__((unused)) = true;
  using __gnu_test::rvalstruct;

  typedef std::set<rvalstruct> Set;
  Set s;
  VERIFY( s.empty() );

  std::pair<Set::iterator, bool> p = s.insert(rvalstruct(1));
  VERIFY( p.second );
  VERIFY( s.size() == 1 );
  VERIFY( std::distance(s.begin(), s.end()) == 1 );
  VERIFY( p.first == s.begin() );
  VERIFY( (*p.first).val == 1 );
}

void test02()
{
  bool test __attribute__((unused)) = true;
  using __gnu_test::rvalstruct;

  typedef std::set<rvalstruct> Set;
  Set s;
  VERIFY( s.empty() );

  std::pair<Set::iterator, bool> p1 = s.insert(rvalstruct(2));
  std::pair<Set::iterator, bool> p2 = s.insert(rvalstruct(2));  
  VERIFY( p1.second );
  VERIFY( !p2.second );
  VERIFY( s.size() == 1 );
  VERIFY( p1.first == p2.first );
  VERIFY( (*p1.first).val == 2 );
}

int main()
{
  test01();
  test02();
  return 0;
}

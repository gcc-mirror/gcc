// { dg-do run { target c++11 } }

// 2010-11-10  Paolo Carlini  <paolo.carlini@oracle.com> 
//
// Copyright (C) 2010-2024 Free Software Foundation, Inc.
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
  using __gnu_test::rvalstruct;

  typedef std::set<rvalstruct> Set;
  Set s;
  VERIFY( s.empty() );

  Set::iterator p = s.insert(s.begin(), rvalstruct(1));
  VERIFY( s.size() == 1 );
  VERIFY( std::distance(s.begin(), s.end()) == 1 );
  VERIFY( p == s.begin() );
  VERIFY( (*p).val == 1 );
}

void test02()
{
  using __gnu_test::rvalstruct;

  typedef std::set<rvalstruct> Set;
  Set s;
  VERIFY( s.empty() );

  Set::iterator p1 = s.insert(s.begin(), rvalstruct(2));
  Set::iterator p2 = s.insert(p1, rvalstruct(2));  
  VERIFY( s.size() == 1 );
  VERIFY( p1 == p2 );
  VERIFY( (*p1).val == 2 );
}

int main()
{
  test01();
  test02();
  return 0;
}

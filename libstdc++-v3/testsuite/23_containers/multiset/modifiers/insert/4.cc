// { dg-do run { target c++11 } }

// 2010-11-10  Paolo Carlini  <paolo.carlini@oracle.com> 
//
// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

  typedef std::multiset<rvalstruct> Set;
  Set s;
  VERIFY( s.empty() );

  Set::iterator i = s.insert(s.begin(), rvalstruct(1));
  VERIFY( s.size() == 1 );
  VERIFY( std::distance(s.begin(), s.end()) == 1 );
  VERIFY( i == s.begin() );
  VERIFY( (*i).val == 1 );
}

void test02()
{
  using __gnu_test::rvalstruct;

  typedef std::multiset<rvalstruct> Set;
  Set s;
  VERIFY( s.empty() );

  Set::iterator i0 = s.insert(s.begin(), rvalstruct(2));
  Set::iterator i1 = s.insert(i0, rvalstruct(2));
  VERIFY( s.size() == 2 );
  VERIFY( std::distance(s.begin(), s.end()) == 2 );
  VERIFY( (*i1).val == 2 );
  
  Set::iterator i2 = s.begin();
  ++i2;
  VERIFY( i1 == s.begin() );
  VERIFY( (*(s.begin())).val == 2 && (*i2).val == 2 );
}

int main()
{
  test01();
  test02();
  return 0;
}

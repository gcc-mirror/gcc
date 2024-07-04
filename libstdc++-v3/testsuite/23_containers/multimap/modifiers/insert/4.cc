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
#include <map>
#include <testsuite_hooks.h>
#include <testsuite_rvalref.h>

void test01()
{
  using __gnu_test::rvalstruct;

  typedef std::multimap<rvalstruct, rvalstruct> Map;
  typedef std::pair<rvalstruct, rvalstruct> Pair;

  Map m;
  VERIFY( m.empty() );

  Map::iterator i = m.insert(m.begin(),
			     Pair(rvalstruct(1), rvalstruct(3)));
  VERIFY( m.size() == 1 );
  VERIFY( std::distance(m.begin(), m.end()) == 1 );
  VERIFY( i == m.begin() );
  VERIFY( (i->first).val == 1 );
  VERIFY( (i->second).val == 3 );
}

void test02()
{
  using __gnu_test::rvalstruct;

  typedef std::multimap<rvalstruct, rvalstruct> Map;
  typedef std::pair<rvalstruct, rvalstruct> Pair;

  Map m;
  VERIFY( m.empty() );

  Map::iterator i0 = m.insert(Pair(rvalstruct(2), rvalstruct(3)));
  m.insert(i0, Pair(rvalstruct(2), rvalstruct(7)));

  VERIFY( m.size() == 2 );
  VERIFY( std::distance(m.begin(), m.end()) == 2 );

  Map::iterator i1 = m.begin();
  Map::iterator i2 = i1;
  ++i2;

  VERIFY( (i1->first).val == 2 );
  VERIFY( (i2->first).val == 2 );
  VERIFY( (i1->second).val == 7 && (i2->second).val == 3 );
}

int main()
{
  test01();
  test02();
  return 0;
}

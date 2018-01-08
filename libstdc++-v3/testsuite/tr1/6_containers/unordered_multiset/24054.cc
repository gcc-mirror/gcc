// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// 6.3 Unordered associative containers

#include <tr1/unordered_set>
#include <string>
#include <testsuite_hooks.h>

// libstdc++/24054
void test01()
{
  typedef std::tr1::unordered_multiset<std::string> Set;

  Set s;

  s.insert("etaoin");
  s.insert("etaoin");
  s.insert("etaoin");
  s.insert("shrdlu");

  VERIFY( s.erase("") == 0 );
  VERIFY( s.size() == 4 );

  VERIFY( s.erase("etaoin") == 3 );
  VERIFY( s.size() == 1 );

  VERIFY( s.erase("shrdlu") == 1 );
  VERIFY( s.size() == 0 );
}

int main()
{
  test01();
  return 0;
}

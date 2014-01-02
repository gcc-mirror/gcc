// { dg-do compile }

// Copyright (C) 2000-2014 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 23.3.4 template class multiset negative tests
// 2000-09-07 bgarcia@laurelnetworks.com

#include <map>
#include <string>

// libstdc++/86: map & set iterator comparisons are not type-safe
void test01()
{
  bool test __attribute__((unused)) = true;
  std::map<unsigned int, int> mapByIndex;
  std::map<std::string, unsigned> mapByName;
  
  mapByIndex.insert(std::pair<unsigned, int>(0, 1));
  mapByIndex.insert(std::pair<unsigned, int>(6, 5));
  
  std::map<unsigned, int>::iterator itr(mapByIndex.begin());

  // NB: notice, it's not mapByIndex!!
  test &= itr != mapByName.end(); // { dg-error "no" } 
  test &= itr == mapByName.end(); // { dg-error "no" } 
}


// { dg-do run }

// 2005-2-18  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// 6.3.4.4 unordered_map
// find, equal_range, count

#include <string>
#include <iterator>
#include <algorithm>
#include <utility>
#include <tr1/unordered_map>
#include "testsuite_hooks.h"

void test01()
{
  typedef std::tr1::unordered_map<std::string, int> Map;
  typedef std::pair<const std::string, int> Pair;

  Map m;
  VERIFY(m.empty());

  std::pair<Map::iterator, bool> tmp = m.insert(Pair("grape", 3));
  Map::iterator i = tmp.first;
  VERIFY(tmp.second);

  Map::iterator i2 = m.find("grape");
  VERIFY(i2 != m.end());
  VERIFY(i2 == i);
  VERIFY(i2->first == "grape");
  VERIFY(i2->second == 3);

  Map::iterator i3 = m.find("lime");
  VERIFY(i3 == m.end());

  std::pair<Map::iterator, Map::iterator> p = m.equal_range("grape");
  VERIFY(std::distance(p.first, p.second) == 1);
  VERIFY(p.first == i2);

  std::pair<Map::iterator, Map::iterator> p2 = m.equal_range("lime");
  VERIFY(p2.first == p2.second);

  VERIFY(m.count("grape") == 1);
  VERIFY(m.count("lime") == 0);
}

int main()
{
  test01();
  return 0;
}

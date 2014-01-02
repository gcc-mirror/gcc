// { dg-do run }

// 2005-2-18  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

// 6.3.4.6 unordered_multimap
// find, equal_range, count

#include <string>
#include <iterator>
#include <algorithm>
#include <utility>
#include <tr1/unordered_map>
#include "testsuite_hooks.h"

bool test __attribute__((unused)) = true;

void test01()
{
  typedef std::tr1::unordered_multimap<std::string, int> Map;
  typedef std::pair<const std::string, int> Pair;

  Map m;
  VERIFY(m.empty());

  m.insert(Pair("grape", 3));
  m.insert(Pair("durian", 8));
  m.insert(Pair("grape", 7));

  Map::iterator i1 = m.find("grape");
  Map::iterator i2 = m.find("durian");
  Map::iterator i3 = m.find("kiwi");

  VERIFY(i1 != m.end());
  VERIFY(i1->first == "grape");
  VERIFY(i1->second == 3 || i2->second == 7);
  VERIFY(i2 != m.end());
  VERIFY(i2->first == "durian");
  VERIFY(i2->second == 8);
  VERIFY(i3 == m.end());

  std::pair<Map::iterator, Map::iterator> p1 = m.equal_range("grape");
  VERIFY(std::distance(p1.first, p1.second) == 2);
  Map::iterator tmp = p1.first;
  ++tmp;
  VERIFY(p1.first->first == "grape");
  VERIFY(tmp->first == "grape");
  VERIFY((p1.first->second == 3 && tmp->second == 7) ||
	 (p1.first->second == 7 && tmp->second == 3));

  std::pair<Map::iterator, Map::iterator> p2 = m.equal_range("durian");
  VERIFY(std::distance(p2.first, p2.second) == 1);
  VERIFY(p2.first->first == "durian");
  VERIFY(p2.first->second == 8);

  std::pair<Map::iterator, Map::iterator> p3 = m.equal_range("kiwi");
  VERIFY(p3.first == p3.second);

  VERIFY(m.count("grape") == 2);
  VERIFY(m.count("durian") == 1);
  VERIFY(m.count("kiwi") == 0);
}

int main()
{
  test01();
  return 0;
}

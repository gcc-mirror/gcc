// { dg-do run }

// 2005-2-17  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2005-2020 Free Software Foundation, Inc.
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
// Single-element insert

#include <string>
#include <iterator>
#include <tr1/unordered_map>
#include "testsuite_hooks.h"

void test01()
{
  typedef std::tr1::unordered_map<std::string, int> Map;
  typedef std::pair<const std::string, int> Pair;

  Map m;
  VERIFY(m.empty());

  std::pair<Map::iterator, bool> p = m.insert(Pair("abcde", 3));
  VERIFY(p.second);
  VERIFY(m.size() == 1);
  VERIFY(std::distance(m.begin(), m.end()) == 1);
  VERIFY(p.first == m.begin());
  VERIFY(p.first->first == "abcde");
  VERIFY(p.first->second == 3);
}

void test02()
{
  typedef std::tr1::unordered_map<std::string, int> Map;
  typedef std::pair<const std::string, int> Pair;

  Map m;
  VERIFY(m.empty());

  std::pair<Map::iterator, bool> p1 = m.insert(Pair("abcde", 3));
  std::pair<Map::iterator, bool> p2 = m.insert(Pair("abcde", 7));

  VERIFY(p1.second);
  VERIFY(!p2.second);
  VERIFY(m.size() == 1);
  VERIFY(p1.first == p2.first);
  VERIFY(p1.first->first == "abcde");
  VERIFY(p2.first->second == 3);
}

int main()
{
  test01();
  test02();
  return 0;
}

// { dg-do run }

// 2005-2-17  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2005 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 6.3.4.6 unordered_multimap
// Single-element insert

#include <string>
#include <iterator>
#include <tr1/unordered_map>
#include "testsuite_hooks.h"

bool test __attribute__((unused)) = true;

void test01()
{
  typedef std::tr1::unordered_multimap<std::string, int> Map;
  typedef std::pair<const std::string, int> Pair;

  Map m;
  VERIFY(m.empty());

  Map::iterator i = m.insert(Pair("abcde", 3));
  VERIFY(m.size() == 1);
  VERIFY(std::distance(m.begin(), m.end()) == 1);
  VERIFY(i == m.begin());
  VERIFY(i->first == "abcde");
  VERIFY(i->second == 3);
}

void test02()
{
  typedef std::tr1::unordered_multimap<std::string, int> Map;
  typedef std::pair<const std::string, int> Pair;

  Map m;
  VERIFY(m.empty());

  m.insert(Pair("abcde", 3));
  m.insert(Pair("abcde", 7));

  VERIFY(m.size() == 2);
  VERIFY(std::distance(m.begin(), m.end()) == 2);

  Map::iterator i1 = m.begin();
  Map::iterator i2 = i1;
  ++i2;

  VERIFY(i1->first == "abcde");
  VERIFY(i2->first == "abcde");
  VERIFY((i1->second == 3 && i2->second == 7) ||
	 (i1->second == 7 && i2->second == 3));
}

int main()
{
  test01();
  test02();
  return 0;
}

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

// 6.3.4.5 unordered_set
// find, equal_range, count

#include <string>
#include <iterator>
#include <algorithm>
#include <tr1/unordered_set>
#include "testsuite_hooks.h"

void test01()
{
  typedef std::tr1::unordered_multiset<std::string> Set;
  Set s;
  VERIFY(s.empty());

  s.insert("grape");
  s.insert("banana");
  s.insert("grape");

  Set::iterator i2 = s.find("banana");
  VERIFY(i2 != s.end());
  VERIFY(*i2 == "banana");

  std::pair<Set::iterator, Set::iterator> p = s.equal_range("grape");
  VERIFY(std::distance(p.first, p.second) == 2);
  Set::iterator i3 = p.first;
  ++i3;
  VERIFY(*p.first == "grape");
  VERIFY(*i3 == "grape");

  Set::iterator i4 = s.find("lime");
  VERIFY(i4 == s.end());

  VERIFY(s.count("grape") == 2);
  VERIFY(s.count("banana") == 1);
  VERIFY(s.count("lime") == 0);
}

int main()
{
  test01();
  return 0;
}

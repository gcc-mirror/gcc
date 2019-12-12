// { dg-do run }

// 2005-2-17  Matt Austern  <austern@apple.com>
//
// Copyright (C) 2005-2019 Free Software Foundation, Inc.
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

// 6.3.4.3 unordered_set
// Single-element insert

#include <string>
#include <iterator>
#include <tr1/unordered_set>
#include "testsuite_hooks.h"

void test01()
{
  typedef std::tr1::unordered_set<std::string> Set;
  Set s;
  VERIFY(s.empty());

  std::pair<Set::iterator, bool> p = s.insert("abcde");
  VERIFY(p.second);
  VERIFY(s.size() == 1);
  VERIFY(std::distance(s.begin(), s.end()) == 1);
  VERIFY(p.first == s.begin());
  VERIFY(*p.first == "abcde");
}

void test02()
{
  typedef std::tr1::unordered_set<std::string> Set;
  Set s;
  VERIFY(s.empty());

  std::pair<Set::iterator, bool> p1 = s.insert("abcde");
  std::pair<Set::iterator, bool> p2 = s.insert("abcde");
  VERIFY(p1.second);
  VERIFY(!p2.second);
  VERIFY(s.size() == 1);
  VERIFY(p1.first == p2.first);
  VERIFY(*p1.first == "abcde");
}

int main()
{
  test01();
  test02();
  return 0;
}

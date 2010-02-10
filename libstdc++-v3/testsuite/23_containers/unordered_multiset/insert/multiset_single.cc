// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010 Free Software Foundation, Inc.
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

// Single-element insert

#include <string>
#include <iterator>
#include <unordered_set>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::unordered_multiset<std::string> Set;
  Set s;
  VERIFY(s.empty());

  Set::iterator i = s.insert("abcde");
  VERIFY(s.size() == 1);
  VERIFY(std::distance(s.begin(), s.end()) == 1);
  VERIFY(i == s.begin());
  VERIFY(*i == "abcde");
}

void test02()
{
  bool test __attribute__((unused)) = true;

  typedef std::unordered_multiset<std::string> Set;
  Set s;
  VERIFY(s.empty());

  s.insert("abcde");
  Set::iterator i = s.insert("abcde");
  VERIFY(s.size() == 2);
  VERIFY(std::distance(s.begin(), s.end()) == 2);
  VERIFY(*i == "abcde");
  
  Set::iterator i2 = s.begin();
  ++i2;
  VERIFY(i == s.begin() || i == i2);
  VERIFY(*(s.begin()) == "abcde" && *i2 == "abcde");
}

int main()
{
  test01();
  test02();
  return 0;
}

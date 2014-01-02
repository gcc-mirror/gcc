// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010-2014 Free Software Foundation, Inc.
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

// range insert

#include <string>
#include <iterator>
#include <algorithm>
#include <unordered_map>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::unordered_map<std::string, int> Map;
  typedef std::pair<const std::string, int> Pair;

  Map m;
  VERIFY(m.empty());

  Pair A[5] =
    {
      Pair("red", 5),
      Pair("green", 9),
      Pair("blue", 3),
      Pair("cyan", 8),
      Pair("magenta", 7)
    };

  m.insert(A+0, A+5);
  VERIFY(m.size() == 5);
  VERIFY(std::distance(m.begin(), m.end()) == 5);

  VERIFY(m["red"] == 5);
  VERIFY(m["green"] == 9);
  VERIFY(m["blue"] == 3);
  VERIFY(m["cyan"] == 8);
  VERIFY(m["magenta"] == 7);
}

void test02()
{
  bool test __attribute__((unused)) = true;

  typedef std::unordered_map<std::string, int> Map;
  typedef std::pair<const std::string, int> Pair;

  Map m;
  VERIFY(m.empty());

  Pair A[9] =
    {
      Pair("red", 5),
      Pair("green", 9),
      Pair("red", 19),
      Pair("blue", 3),
      Pair("blue", 60),
      Pair("cyan", 8),
      Pair("magenta", 7),
      Pair("blue", 99),
      Pair("green", 33)
    };

  m.insert(A+0, A+9);
  VERIFY(m.size() == 5);
  VERIFY(std::distance(m.begin(), m.end()) == 5);

  VERIFY(m["red"] == 5);
  VERIFY(m["green"] == 9);
  VERIFY(m["blue"] == 3);
  VERIFY(m["cyan"] == 8);
  VERIFY(m["magenta"] == 7);
}

int main()
{
  test01();
  test02();
  return 0;
}

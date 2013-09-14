// { dg-options "-std=gnu++11" }
// { dg-do run { xfail *-*-* } }

//
// 2013-09-14  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013 Free Software Foundation, Inc.
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

// 28.11.3 regex_search
// Tests ECMAScript assertion.

#include <regex>
#include <testsuite_hooks.h>

using namespace std;

void
test01()
{
  bool test __attribute__((unused)) = true;

  VERIFY(!regex_search("2123456", regex("^1234")));
  VERIFY(regex_search("123456", regex("^1234")));
  VERIFY(regex_search("123456", regex("(5|^)1234")));
  VERIFY(regex_search("5123456", regex("(5|^)1234")));
  VERIFY(!regex_search("1234562", regex("3456$")));
  VERIFY(regex_search("123456", regex("3456$")));
  VERIFY(!regex_search("123456", regex("(?=1234)56")));
  VERIFY(regex_search("123456", regex("(?=1234)123456")));
  VERIFY(regex_search("123456", regex("(?!1234)56")));
  VERIFY(!regex_search("123456", regex("(?!1234)123456")));

  VERIFY(regex_search("a-", regex("a\\b-")));
  VERIFY(!regex_search("ab", regex("a\\bb")));
  VERIFY(!regex_search("a-", regex("a\\B-")));
  VERIFY(regex_search("ab", regex("a\\Bb")));

  string s("This is a regular expression");
  string sol[] =
    {
      "This",
      "is",
      "a",
      "regular",
      "expression",
    };

  regex re("\\b\\w*\\b");
  int i = 0;
  for (auto it = sregex_iterator(s.begin(), s.end(), re);
       it != sregex_iterator() && i < 5;
       ++it)
    {
      string s((*it)[0].first, (*it)[0].second);
      VERIFY(s == sol[i++]);
    }
  VERIFY(i == 5);
}

int
main()
{
  test01();
  return 0;
}

// { dg-do run { target c++11 } }

//
// 2013-09-14  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013-2016 Free Software Foundation, Inc.
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
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

void
test01()
{
  bool test __attribute__((unused)) = true;

  VERIFY(!regex_search_debug("2123456", regex("^1234")));
  VERIFY(regex_search_debug("123456", regex("^1234")));
  VERIFY(regex_search_debug("123456", regex("(5|^)1234")));
  VERIFY(regex_search_debug("5123456", regex("(5|^)1234")));
  VERIFY(!regex_search_debug("1234562", regex("3456$")));
  VERIFY(regex_search_debug("123456", regex("3456$")));
  VERIFY(!regex_search_debug("123456", regex("(?=1234)56")));
  VERIFY(regex_search_debug("123456", regex("(?=1234)123456")));
  VERIFY(regex_search_debug("123456", regex("(?!1234)56")));
  VERIFY(!regex_search_debug("123456", regex("(?!1234)123456")));

  VERIFY(regex_search_debug("a-", regex("a\\b-")));
  VERIFY(!regex_search_debug("ab", regex("a\\bb")));
  VERIFY(!regex_search_debug("a-", regex("a\\B-")));
  VERIFY(regex_search_debug("ab", regex("a\\Bb")));

  string s("This is a regular expression");
  string sol[] =
    {
      "This",
      "",
      "is",
      "",
      "a",
      "",
      "regular",
      "",
      "expression",
      "",
    };

  regex re("\\b\\w*\\b");
  int i = 0;
  for (auto it = sregex_iterator(s.begin(), s.end(), re);
       it != sregex_iterator();
       ++it)
    {
      string s((*it)[0].first, (*it)[0].second);
      VERIFY(s == sol[i++]);
    }
  VERIFY(i == 10);

  {
    cmatch m;
    regex re("(?=(as)df)as(df)");
    regex_search("asdf", m, re);
    VERIFY(m.size() == 3);
    VERIFY(m[0].matched && string(m[0].first, m[0].second) == "asdf");
    VERIFY(m[1].matched && string(m[1].first, m[1].second) == "as");
    VERIFY(m[2].matched && string(m[2].first, m[2].second) == "df");
  }
}

int
main()
{
  test01();
  return 0;
}

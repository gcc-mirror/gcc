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
// Tests ECMAScript greedy and ungreedy quantifiers.

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

void
test01()
{
  cmatch m;
#define TEST(i, s) VERIFY(m[i].matched && string(m[i].first, m[i].second) == s)
  VERIFY(regex_search_debug("aaaa", m, regex("a*")));
  TEST(0, "aaaa");
  VERIFY(regex_search_debug("aaaa", m, regex("a*?")));
  TEST(0, "");
  VERIFY(regex_search_debug("aaaa", m, regex("a+")));
  TEST(0, "aaaa");
  VERIFY(regex_search_debug("aaaa", m, regex("a+?")));
  TEST(0, "a");
  VERIFY(regex_search_debug("a", m, regex("a?")));
  TEST(0, "a");
  VERIFY(regex_search_debug("a", m, regex("a??")));
  TEST(0, "");
  VERIFY(regex_search_debug("", m, regex("a??")));
  TEST(0, "");
  VERIFY(regex_search_debug("aaaa", m, regex("(a+)(a+)")));
  TEST(1, "aaa");
  TEST(2, "a");
  VERIFY(regex_search_debug("aaaa", m, regex("(a+)(a+?)")));
  TEST(1, "aaa");
  TEST(2, "a");
  VERIFY(regex_search_debug("aaaa", m, regex("(a+?)(a+)")));
  TEST(1, "a");
  TEST(2, "aaa");
  VERIFY(regex_search_debug("aaaa", m, regex("(a+?)(a+?)")));
  TEST(1, "a");
  TEST(2, "a");
}

int
main()
{
  test01();
  return 0;
}

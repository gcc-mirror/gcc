// { dg-do run { target c++11 } }

//
// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

// PR libstdc++/67362
void
test01()
{
  regex re("((.)", regex_constants::basic);
}

void
test02()
{
  std::string re_str
    {
      "/abcd" "\n"
      "/aecf" "\n"
      "/ghci"
    };
  auto rx = std::regex(re_str, std::regex_constants::grep | std::regex_constants::icase);
  VERIFY(regex_search_debug("/abcd", rx));
}

void
test03()
{
  VERIFY(regex_match_debug("a.", regex(R"(a\b.)"), regex_constants::match_not_eow));
  VERIFY(regex_match_debug(".a", regex(R"(.\ba)"), regex_constants::match_not_bow));
  VERIFY(regex_search_debug("a", regex(R"(^\b)")));
  VERIFY(regex_search_debug("a", regex(R"(\b$)")));
  VERIFY(!regex_search_debug("a", regex(R"(^\b)"), regex_constants::match_not_bow));
  VERIFY(!regex_search_debug("a", regex(R"(\b$)"), regex_constants::match_not_eow));
}

// PR libstdc++/77356
void
test04()
{
  static const char* kNumericAnchor ="(\\$|usd)(usd|\\$|to|and|up to|[0-9,\\.\\-\\sk])+";
  const std::regex re(kNumericAnchor);
  (void)re;
}

void
test05()
{
  VERIFY(regex_match_debug("!", std::regex("[![:alnum:]]")));
  VERIFY(regex_match_debug("-", std::regex("[a-]", regex_constants::basic)));
  VERIFY(regex_match_debug("-", std::regex("[a-]")));
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  return 0;
}


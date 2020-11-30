// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2013-09-18  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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
// Tests ECMAScript flags.

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

void
test01()
{
  cmatch m;
  regex re("((as)(df))", regex_constants::ECMAScript | regex_constants::nosubs);
  VERIFY(regex_search_debug("asdf", m, re));
  VERIFY(m.size() == 1);
  VERIFY(m[0].matched && string(m[0].first, m[0].second) == "asdf");

  VERIFY( regex_search_debug("a", regex("^a")));
  VERIFY(!regex_search_debug("a", regex("^a"), regex_constants::match_not_bol));
  VERIFY( regex_search_debug("a", regex("a$")));
  VERIFY(!regex_search_debug("a", regex("a$"), regex_constants::match_not_eol));
  VERIFY( regex_search_debug("a", regex("\\ba")));
  VERIFY(!regex_search_debug("a", regex("\\ba"),
			     regex_constants::match_not_bow));
  VERIFY( regex_search_debug("a", regex("a\\b")));
  VERIFY(!regex_search_debug("a", regex("a\\b"),
			     regex_constants::match_not_eow));
  VERIFY( regex_search_debug("", regex("")));
  VERIFY(!regex_search_debug("", regex(""), regex_constants::match_not_null));
  VERIFY( regex_search_debug("", regex("^$")));
  VERIFY(!regex_search_debug("", regex("^$"), regex_constants::match_not_null));
  VERIFY( regex_search_debug("aaa", m, regex("a*?"),
			     regex_constants::match_not_null));
  VERIFY(m[0].matched && string(m[0].first, m[0].second) == "a");
  VERIFY( regex_search_debug("asdf", regex("sdf")));
  VERIFY(!regex_search_debug("asdf", regex("sdf"),
			     regex_constants::match_continuous));
  VERIFY( regex_search_debug(" a"+1, regex("\\ba"),
			     regex_constants::match_prev_avail));
  VERIFY( regex_search_debug("ba"+1, regex("\\Ba"),
			     regex_constants::match_prev_avail));
  // PR libstdc++/63920
  VERIFY(!regex_search_debug("a", regex("b*"), regex_constants::match_not_null));
}

int
main()
{
  test01();
  return 0;
}

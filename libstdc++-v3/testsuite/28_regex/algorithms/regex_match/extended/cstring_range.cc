// { dg-do run { target c++11 } }

//
// 2013-09-05  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013-2017 Free Software Foundation, Inc.
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

// 28.11.2 regex_match
// Tests Extended interval range.

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

void
test01()
{
  regex re;
  re.assign("(ab){3}", std::regex::extended);
  VERIFY(!regex_match_debug("abab", re));
  VERIFY(regex_match_debug("ababab", re));
  VERIFY(!regex_match_debug("abababab", re));
  re.assign("(ab){3,}", std::regex::extended);
  VERIFY(!regex_match_debug("abab", re));
  VERIFY(regex_match_debug("ababab", re));
  VERIFY(regex_match_debug("abababab", re));
  VERIFY(regex_match_debug("ababababab", re));
  re.assign("(ab){0,3}", std::regex::extended);
  VERIFY(regex_match_debug("", re));
  VERIFY(regex_match_debug("ab", re));
  VERIFY(regex_match_debug("abab", re));
  VERIFY(regex_match_debug("ababab", re));
  VERIFY(!regex_match_debug("abababab", re));
  re.assign("(a|b){0,2}", std::regex::extended);
  VERIFY(regex_match_debug("", re));
  VERIFY(regex_match_debug("a", re));
  VERIFY(regex_match_debug("b", re));
  VERIFY(regex_match_debug("aa", re));
  VERIFY(regex_match_debug("ab", re));
  VERIFY(regex_match_debug("ba", re));
  VERIFY(regex_match_debug("bb", re));
  VERIFY(!regex_match_debug("aaa", re));
}

int
main()
{
  test01();
  return 0;
}

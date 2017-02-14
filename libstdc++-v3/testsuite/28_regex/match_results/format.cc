// { dg-do run { target c++11 } }

//
// 2013-09-24  Tim Shen <timshen91@gmail.com>
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

// 28.10.5 formatting
// Tests ECMAScript format()

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace std;
using namespace __gnu_test;

void
test01()
{
  cmatch m;
  VERIFY(regex_search_debug("*** this is a string !!!", m,
			    regex("(\\w+) (\\w+) (\\w+) (\\w+)")));
  VERIFY(m.format("$&|$`|$3|$4|$2|$1|$'$$$")
	 == "this is a string|*** |a|string|is|this| !!!$$");
  VERIFY(m.format("&|\\3|\\4|\\2|\\1|\\",
		  regex_constants::format_sed)
	 == "this is a string|a|string|is|this|\\");
}

void
test02()
{
  regex re("asdf");
  cmatch m;
  regex_match("asdf", m, re);
  string fmt = "|&|\\0|";
  char buff[4096] = {0};
  string res = "|asdf|asdf|";
  VERIFY(m.format(buff, fmt.data(), fmt.data() + fmt.size(),
		  regex_constants::format_sed) == buff + res.size());
  VERIFY(res == buff);
}

int
main()
{
  test01();
  test02();
  return 0;
}

// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

// 28.11.4 regex_replace
// Tests ECMAScript regex_replace's _Out_iter return value.

#include <regex>
#include <testsuite_hooks.h>

using namespace std;

void
test01()
{
  char buff[4096] = {0};
  regex re("asdf");
  cmatch m;
  string s = "asdf";
  string res = "|asdf|asdf|";
  VERIFY(regex_replace(buff, s.data(), s.data() + s.size(), re, "|&|\\0|",
		       regex_constants::format_sed) == buff + res.size());
  VERIFY(res == buff);
}

int
main()
{
  test01();
  return 0;
}

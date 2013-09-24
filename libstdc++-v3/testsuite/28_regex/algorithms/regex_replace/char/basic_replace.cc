// { dg-options "-std=gnu++11" }

//
// 2013-09-24  Tim Shen <timshen91@gmail.com>
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

// 28.11.4 regex_replace
// Tests ECMAScript regex_replace.

#include <regex>
#include <testsuite_hooks.h>

using namespace std;

void
test01()
{
  bool test __attribute__((unused)) = true;

  VERIFY(regex_replace(string("This is a string"), regex("\\b\\w*\\b"), "|$0|")
	 == "|This||| |is||| |a||| |string|||");
  VERIFY(regex_replace(string("This is a string"), regex("\\b\\w*\\b"), "|$0|",
		       regex_constants::format_no_copy)
	 == "|This||||is||||a||||string|||");
  VERIFY(regex_replace(string("This is a string"), regex("\\b\\w*\\b"), "|$0|",
		       regex_constants::format_first_only)
	 == "|This| is a string");
}

int
main()
{
  test01();
  return 0;
}

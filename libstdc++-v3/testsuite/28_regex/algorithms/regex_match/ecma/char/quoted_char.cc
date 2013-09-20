// { dg-options "-std=gnu++11" }

//
// 2013-09-05  Tim Shen <timshen91@gmail.com>
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

// 28.11.2 regex_match
// Tests ECMAScript \d \D \s \S \w \W

#include <regex>
#include <testsuite_hooks.h>

using namespace std;

void
test01()
{
  bool test __attribute__((unused)) = true;

  VERIFY(regex_match("01", regex("\\d*")));
  VERIFY(regex_match("asdfjkl", regex("\\D*")));
  VERIFY(!regex_match("asdfjkl0", regex("\\D*")));
  VERIFY(regex_match("\r\t\v\f ", regex("\\s*")));
  VERIFY(regex_match("asdfjkl", regex("\\S*")));
  VERIFY(!regex_match("asdfjkl\r", regex("\\S*")));
  VERIFY(regex_match("_az", regex("\\w*")));
  VERIFY(regex_match("!@#$%", regex("\\W*")));
  VERIFY(!regex_match("_01234", regex("\\W*")));
}

int
main()
{
  test01();
  return 0;
}

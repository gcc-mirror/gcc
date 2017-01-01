// { dg-do run { target c++11 } }
// { dg-require-namedlocale "de_DE.UTF-8" }

//
// 2013-08-29  Tim Shen <timshen91@gmail.com>
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
// Tests Extended localization against a wide-string.

#include <regex>
#include <testsuite_hooks.h>
#include <testsuite_regex.h>

using namespace __gnu_test;
using namespace std;

void
test01()
{
  std::wstring str2 = L"ÜBER";
  std::wregex re2;
  re2.imbue(std::locale("de_DE.UTF-8"));
  re2.assign(L"[[:upper:]]*", std::regex::extended);
  std::wsmatch m2;
  VERIFY(regex_match_debug(str2, m2, re2));
}

int
main()
{
  test01();
  return 0;
}

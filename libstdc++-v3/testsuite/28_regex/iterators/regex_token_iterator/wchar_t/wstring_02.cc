// { dg-do run { target c++11 } }
// { dg-require-namedlocale "en_US.UTF-8" }

//
// 2013-08-29  Tim Shen <timshen91@gmail.com>
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

// 28.12.2 regex_token_iterator
// Tests regex_token_iterator class over a localized wstring.

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  std::setlocale(LC_ALL, "en_US.UTF-8");

  std::wstring str2 = L"öäü";
  std::wregex re2;
  re2.assign(L"([[:lower:]]+)");
  std::wsmatch m2;

  std::wsregex_token_iterator end {};
  std::wsregex_token_iterator p{str2.begin(), str2.end(), re2, {1}};

  VERIFY(p == end);
}

int
main()
{
  test01();
  return 0;
}

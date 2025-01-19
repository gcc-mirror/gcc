// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }
// { dg-require-namedlocale "en_US.UTF-8" }

//
// 2013-09-05  Tim Shen <timshen91@gmail.com>
//
// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

// 28.12.1 regex_iterator
// Tests regex_iterator class

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  std::setlocale(LC_ALL, "en_US.UTF-8");

  std::wstring str2 = L"ä\u2009Ä\u2009ö\u2009Ö\u2009ü\u2009Ü";

  std::wregex re2;
  re2.imbue(std::locale("en_US.UTF-8"));

  re2.assign(L"([[:lower:]]{0,1}[[:space:]]{0,1}[[:upper:]]{0,1})");

  std::wstring sol[] =
    {
      L"ä\u2009Ä",
      L"\u2009",
      L"ö\u2009Ö",
      L"\u2009",
      L"ü\u2009Ü",
      L"",
    };
  int i = 0;
  for (std::wsregex_iterator p(str2.begin(), str2.end(), re2);
      p != std::wsregex_iterator{}; ++p)
    VERIFY(std::wstring((*p)[1].first, (*p)[1].second) == sol[i++]);
}

int
main()
{
  test01();
  return 0;
}

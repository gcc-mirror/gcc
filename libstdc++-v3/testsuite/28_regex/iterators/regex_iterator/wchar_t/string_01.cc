// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// 2013-07-20  Tim Shen <timshen91@gmail.com>
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

// 28.12.1 regex_iterator
// Tests operator++() of regex_iterator class

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  std::basic_regex<wchar_t> re(L"asdf");
  const wchar_t s[] = L"ffasdf88asdf99asdf00asdf77";
  int cnt = 0;
  for (std::regex_iterator<const wchar_t*> it(s, *(&s+1)-1, re), e;
       it != e; ++it)
    {
      VERIFY( it->size() == 1 );
      VERIFY( std::wstring((*it)[0].first, (*it)[0].second) == L"asdf" );
      cnt++;
    }
  VERIFY( cnt == 4 );
}

int
main()
{
  test01();
  return 0;
}

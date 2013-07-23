// { dg-options "-std=gnu++11" }

//
// 2013-07-20  Tim Shen <timshen91@gmail.com>
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

// 28.12.2 regex_token_iterator
// Tests "split" of regex_token_iterator class

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::basic_regex<wchar_t> re(L" ");
  const wchar_t s[] = L"Quick brown fox.";
  int cnt = 0;
  std::wcregex_token_iterator iter(s, *(&s+1)-1, re, -1);
  VERIFY( std::wstring(iter->first, iter->second) == L"Quick" );
  ++iter;
  VERIFY( std::wstring(iter->first, iter->second) == L"brown" );
  ++iter;
  VERIFY( std::wstring(iter->first, iter->second) == L"fox." );
  ++iter;
  VERIFY( iter == std::wcregex_token_iterator() );
}

int
main()
{
  test01();
  return 0;
}

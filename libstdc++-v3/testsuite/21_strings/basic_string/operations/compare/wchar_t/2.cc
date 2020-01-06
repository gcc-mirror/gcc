// { dg-options "-std=gnu++17" }

// Copyright (C) 2016-2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// [string::compare]

#include <string>
#include <testsuite_hooks.h>

void
test03()
{
  std::wstring_view str1(L"foobar");
  std::wstring str2(L"foobar");

  auto x = str2.compare(str1);
  VERIFY (x == 0);

  str1 = L"bar";
  x = str2.compare(str1);
  VERIFY (x > 0);

  str1 = L"foobar";
  str2 = L"bar";
  x = str2.compare(str1);
  VERIFY (x < 0);
  x = str2.compare(0, 3, str1, 3, 3);
  VERIFY (x == 0);

  str1 = L"bar";
  str2 = L"foobar";
  x = str2.compare(3, 3, str1);
  VERIFY (x == 0);
}

// PR libstdc++/77264
void
test04()
{
  const std::wstring str(L"a");

  wchar_t c = L'a';
  int res = str.compare(0, 1, &c, 1);
  VERIFY ( !res );

  wchar_t arr[] = L"a";
  res = str.compare(0, 1, arr, 1);
  VERIFY ( !res );

  const wchar_t carr[] = L"a";
  res = str.compare(0, 1, carr, 1);
  VERIFY ( !res );

  struct S {
    operator wchar_t*() { return &c; }
    operator std::wstring_view() { return L"!"; }
    wchar_t c = L'a';
  };

  res = str.compare(0, 1, S{}, 1);
  VERIFY ( !res );
}

int main()
{ 
  test03();
  test04();
  return 0;
}

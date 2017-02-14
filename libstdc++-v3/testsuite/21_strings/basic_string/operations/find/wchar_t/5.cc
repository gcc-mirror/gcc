// { dg-options "-std=gnu++17" }

// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

// [string::find]
// [string::rfind]
// [string::find.first.of]
// [string::find.last.of]
// [string::find.first.not.of]
// [string::find.last.not.of]

#include <string>
#include <testsuite_hooks.h>

void
test03()
{
  std::wstring_view str1(L"bar");
  std::wstring str2(L"foobar");

  auto x = str2.find(str1);
  VERIFY (x == 3);

  x = str2.find(str1, 1);
  VERIFY (x == 3);

  str2 = L"barbar";
  x = str2.rfind(str1);
  VERIFY (x == 3);

  x = str2.rfind(str1, 0);
  VERIFY (x == 0);

  x = str2.rfind(str1, 3);
  VERIFY (x == 3);

  str2 = L"foobarxyz";
  str1 = L"zyx";
  x = str2.find_first_of(str1);
  VERIFY (x == 6);

  str2 = L"foobarxyzfooxyz";
  x = str2.find_first_of(str1);
  VERIFY (x == 6);
  x = str2.find_first_of(str1, 9);
  VERIFY (x == 12);
  x = str2.find_last_of(str1);
  VERIFY (x == 14);
  x = str2.find_last_of(str1, 10);
  VERIFY (x == 8);

  str2 = L"abcabcabcxxx";
  str1 = L"cba";
  x = str2.find_first_not_of(str1);
  VERIFY (x == 9);

  str2 = L"abcabcabcxxxabcabcxxx";
  x = str2.find_first_not_of(str1, 12);
  VERIFY (x == 18);

  str1 = L"x";
  x = str2.find_last_not_of(str1);
  VERIFY (x == 17);
  x = str2.find_last_not_of(str1, 11);
  VERIFY (x == 8);
}

int main()
{ 
  test03();
  return 0;
}

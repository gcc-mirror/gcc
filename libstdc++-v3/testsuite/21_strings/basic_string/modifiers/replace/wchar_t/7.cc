// { dg-options "-std=gnu++17" }

// Copyright (C) 2016 Free Software Foundation, Inc.
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

// [string.modifiers]

#include <string>
#include <testsuite_hooks.h>

void
test03()
{
  bool test __attribute__((unused)) = true;
  std::wstring_view str1(L"foo");
  std::wstring str2(L"bar");
  str2.replace(0, 3, str1);
  VERIFY (str2 == str1);
  str2 = L"foobar";
  str2.replace(3, 3, str1);
  VERIFY (str2 == L"foofoo");
  str2 = L"foobar";
  str2.replace(3, 3, str1, 0, 3);
  VERIFY (str2 == L"foofoo");
  str2 = L"foobar";
  str2.replace(3, 1024, str1, 0, 3);
  VERIFY (str2 == L"foofoo");
  str2 = L"foobar";
  str2.replace(3, 3, str1, 0, 1024);
  VERIFY (str2 == L"foofoo");
  str2 = L"foobar";
  str2.replace(str2.begin()+3, str2.begin()+6, str1);
  VERIFY (str2 == L"foofoo");
}

int main()
{ 
  test03();
  return 0;
}

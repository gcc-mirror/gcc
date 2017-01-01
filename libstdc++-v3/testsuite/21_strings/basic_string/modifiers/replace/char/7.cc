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

// [string.modifiers]

#include <string>
#include <testsuite_hooks.h>

void
test03()
{
  std::string_view str1("foo");
  std::string str2("bar");
  str2.replace(0, 3, str1);
  VERIFY (str2 == str1);
  str2 = "foobar";
  str2.replace(3, 3, str1);
  VERIFY (str2 == "foofoo");
  str2 = "foobar";
  str2.replace(3, 3, str1, 0, 3);
  VERIFY (str2 == "foofoo");
  str2 = "foobar";
  str2.replace(3, 1024, str1, 0, 3);
  VERIFY (str2 == "foofoo");
  str2 = "foobar";
  str2.replace(3, 3, str1, 0, 1024);
  VERIFY (str2 == "foofoo");
  str2 = "foobar";
  str2.replace(str2.begin()+3, str2.begin()+6, str1);
  VERIFY (str2 == "foofoo");
}

// PR libstdc++/77264
void
test04()
{
  std::string str("a");
  char c = 'b';
  str.replace(0, 1, &c, 1);
  VERIFY (str[0] == c);

  char arr[] = "c";
  str.replace(0, 1, arr, 1);
  VERIFY (str[0] == arr[0]);

  const char carr[] = "d";
  str.replace(0, 1, carr, 1);
  VERIFY (str[0] == carr[0]);

  struct S {
    operator char*() { return &c; }
    operator std::string_view() { return "!"; }
    char c = 'e';
  };

  str.replace(0, 1, S{}, 1);
  VERIFY (str[0] == S{}.c);
}

int main()
{ 
  test03();
  test04();
  return 0;
}

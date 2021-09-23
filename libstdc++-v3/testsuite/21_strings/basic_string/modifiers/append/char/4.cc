// { dg-do run { target c++17 } }

// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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
  std::string str2;
  str2 += str1;
  VERIFY (str2 == str1);
  std::string str3;
  str3.append(str1);
  VERIFY (str3 == str1);
  std::string str4;
  str4.append(str1, 1, 2);
  VERIFY (str4 == "oo");
}

// PR libstdc++/77264
void
test04()
{
  std::string str("a");
  char c = 'b';
  str.append(&c, 1);
  VERIFY (str[1] == c);

  char arr[] = "c";
  str.append(arr, 1);
  VERIFY (str[2] == arr[0]);

  const char carr[] = "d";
  str.append(carr, 1);
  VERIFY (str[3] == carr[0]);

  struct S {
    operator char*() { return &c; }
    operator std::string_view() { return "!"; }
    char c = 'e';
  };

  str.append(S{}, 1);
  VERIFY (str[4] == S{}.c);
}

int main()
{ 
  test03();
  test04();
  return 0;
}

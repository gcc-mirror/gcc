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

// [string::compare]

#include <string>
#include <testsuite_hooks.h>

void
test03()
{
  std::string_view str1("foobar");
  std::string str2("foobar");

  auto x = str2.compare(str1);
  VERIFY (x == 0);

  str1 = "bar";
  x = str2.compare(str1);
  VERIFY (x > 0);

  str1 = "foobar";
  str2 = "bar";
  x = str2.compare(str1);
  VERIFY (x < 0);
  x = str2.compare(0, 3, str1, 3, 3);
  VERIFY (x == 0);

  str1 = "bar";
  str2 = "foobar";
  x = str2.compare(3, 3, str1);
  VERIFY (x == 0);
}

// PR libstdc++/77264
void
test04()
{
  const std::string str("a");
  char c = 'a';
  int res = str.compare(0, 1, &c, 1);
  VERIFY ( !res );

  char arr[] = "a";
  res = str.compare(0, 1, arr, 1);
  VERIFY ( !res );

  const char carr[] = "a";
  res = str.compare(0, 1, carr, 1);
  VERIFY ( !res );

  struct S {
    operator char*() { return &c; }
    operator std::string_view() { return "!"; }
    char c = 'a';
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

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

// [string::compare]

#include <string>
#include <testsuite_hooks.h>

void
test03()
{
  bool test __attribute__((unused)) = true;
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

int main()
{ 
  test03();
  return 0;
}

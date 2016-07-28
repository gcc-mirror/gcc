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

int main()
{ 
  test03();
  return 0;
}

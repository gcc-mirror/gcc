// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }

// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

#include <ext/vstring.h>
#include <testsuite_hooks.h>

typedef __gnu_cxx::__versa_string<char> string;

void test01()
{
  string s1("string");
  string s2("");
  std::swap(s1, s2);

  VERIFY( s1.c_str()[0] == '\0' );
}

int main()
{
  test01();
  return 0;
}

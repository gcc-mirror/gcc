// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

// [28.8.5] class template basic_regex locale

#include <string>
#include <regex>
#include <testsuite_hooks.h>

// libstdc++/64585
void test01()
{
  static const char s[] = "a";
  std::regex re("a");
  VERIFY(std::regex_search(s, re));

  auto loc = re.imbue(re.getloc());
  VERIFY(!std::regex_search(s, re));
}

int
main()
{
  test01();
  return 0;
}

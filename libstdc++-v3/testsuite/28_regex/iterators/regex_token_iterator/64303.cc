// { dg-do run { target c++11 } }

//
// Copyright (C) 2014-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 28.12.2 Class template regex_token_iterator

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  const std::string s("  111  222  ");
  const std::regex re("\\w+");

  std::sregex_token_iterator it1(s.begin(), s.end(), re), it2(it1), end;

  for (; it1 != end; ++it1, ++it2) {
    VERIFY(it1 == it2);
    VERIFY(*it1 == *it2);
  }
  VERIFY(it2 == end);
}

int
main()
{
  test01();
  return 0;
}

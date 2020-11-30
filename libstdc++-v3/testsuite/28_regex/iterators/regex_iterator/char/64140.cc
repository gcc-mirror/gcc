// { dg-do run { target c++11 } }
// { dg-timeout-factor 2 }

//
// Copyright (C) 2014-2020 Free Software Foundation, Inc.
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

// libstdc++/64140

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  const std::regex e("z*");
  const std::string s("ab");

  auto it = std::sregex_iterator(s.begin(), s.end(), e);
  auto end = std::sregex_iterator();
  VERIFY(it != end);
  VERIFY(!it->prefix().matched);
  ++it;
  VERIFY(it != end);
  VERIFY(it->prefix().matched);
  ++it;
  VERIFY(it != end);
  VERIFY(it->prefix().matched);
  ++it;
  VERIFY(it == end);
}

int
main()
{
  test01();
  return 0;
}

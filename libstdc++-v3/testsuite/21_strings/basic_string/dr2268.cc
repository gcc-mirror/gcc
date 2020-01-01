// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <string>
#include <testsuite_hooks.h>

void
test01()
{
  // PR libstdc++/84087

  std::string s0 = "string";
  std::string s;
  s.append(s0, 2);
  VERIFY( s == "ring" );
  s.assign(s0, 3);
  VERIFY( s == "ing" );
  s.insert(2, s0, 4);
  VERIFY( s == "inngg" );
  s.replace(2, 3, s0, 2);
  VERIFY( s == "inring" );
  VERIFY( s.compare(2, 4, s0, 2) == 0 );
}

int
main()
{
  test01();
}

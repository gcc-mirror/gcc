// { dg-do run { target c++11 } }

//
// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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

#include <regex>
#include <testsuite_hooks.h>

void
test01()
{
  std::cmatch m;
  std::regex_match("a", m, std::regex("a"));
  std::cmatch mm1 = m, mm2;
  mm1.swap(mm2);
  VERIFY(m == mm2);
  std::swap(mm1, mm2);
  VERIFY(m == mm1);
}

int
main()
{
  test01();
  return 0;
}

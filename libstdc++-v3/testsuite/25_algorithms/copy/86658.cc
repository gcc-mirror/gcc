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

// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do run }
// { dg-skip-if "" { *-*-* } { "-D_GLIBCXX_PARALLEL" } }

#include <algorithm>
#include <vector>

void
test01()
{
  int i[1] = { 1 };
  std::vector<int> v(1);
  std::copy(i, i+1, std::inserter(v, v.end()));
}

int
main()
{
  test01();
}

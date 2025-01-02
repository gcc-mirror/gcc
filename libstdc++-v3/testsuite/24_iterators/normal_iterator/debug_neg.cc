// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// Requires C++11 because we check for correct output of
// __gnu_cxx::__normal_iterator which is improved in this mode.
// { dg-do run { target c++11 xfail *-*-* } }

#include <debug/vector>

void test01()
{
  std::vector<int> vals { 0, 1, 2, 3 };
  __gnu_debug::vector<int> vals2(vals.begin() + 1, vals.begin());
}

int main()
{
  test01();
  return 0;
}

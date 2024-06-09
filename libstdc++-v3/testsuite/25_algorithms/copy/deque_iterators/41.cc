// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
//
// { dg-do run { xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <algorithm>
#include <deque>
#include <vector>

#include <testsuite_hooks.h>

void test01()
{
  std::deque<int> d;
  for (int i = 0; i != 1024; ++i)
    d.push_back(i);

  std::vector<int> dest(d.size(), 0);

  std::copy(d.begin() + 10, d.begin() + 5, dest.begin());
}

int main()
{
  test01();
  return 0;
}

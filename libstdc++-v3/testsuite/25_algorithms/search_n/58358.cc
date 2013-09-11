// Copyright (C) 2013 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++11" }

// 25.1.9 [lib.alg.search]

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>

void test01()
{
  bool test __attribute__((unused)) = true;

  std::vector<int> a{2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
  int count = 0;
  std::search_n(a.begin(), a.end(), 10, 1,
		[&count](int t, int u) { ++count; return t == u; });
  VERIFY( count <= 11 );
}

int main()
{
  test01();
  return 0;
}

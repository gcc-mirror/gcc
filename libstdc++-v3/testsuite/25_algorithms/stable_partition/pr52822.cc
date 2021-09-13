// { dg-do run { target c++11 } }

// Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

// 25.2.12 [lib.alg.partitions] Partitions.

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>

bool true_vector_pred(const std::vector<int>&) { return true; }

void
test01()
{
  std::vector<std::vector<int> > v(1);
  v[0].push_back(7);
  VERIFY( v[0].size() == 1 );
  std::stable_partition(v.begin(), v.end(), &true_vector_pred);
  VERIFY( v[0].size() == 1 );
}

int
main()
{
  test01();
  return 0;
}

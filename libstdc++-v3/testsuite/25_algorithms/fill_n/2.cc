// Copyright (C) 2018 Free Software Foundation, Inc.
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

// 25.2.5 [lib.alg.fill] Fill_n.

// { dg-require-debug-mode "" }

#include <algorithm>
#include <vector>

#include <testsuite_hooks.h>

void
test01()
{
  std::vector<int> ref;
  ref.push_back(1);
  ref.push_back(2);

  std::vector<std::vector<int>> vvect;
  vvect.push_back(std::vector<int>());
  vvect.push_back(std::vector<int>());

  VERIFY( std::fill_n(vvect.begin(), 2, ref) == vvect.end() );
}

int
main()
{
  test01();
  return 0;
}

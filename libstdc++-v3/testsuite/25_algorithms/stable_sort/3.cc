// Copyright (C) 2012-2018 Free Software Foundation, Inc.
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

// 25.3.1.2 [lib.stable.sort]

#include <vector>
#include <algorithm>
#include <testsuite_hooks.h>

void 
test1()
{
  std::vector<bool> bools;
  bools.push_back(true);
  bools.push_back(false);
  bools.push_back(true);
  bools.push_back(false);
  std::stable_sort(bools.begin(), bools.end());
  VERIFY( !bools[0] && !bools[1] && bools[2] && bools[3] );
}

int 
main()
{
  test1();
}

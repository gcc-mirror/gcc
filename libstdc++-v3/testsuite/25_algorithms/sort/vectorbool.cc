// Copyright (C) 2005-2014 Free Software Foundation, Inc.
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

// 25.3.1 algorithms, sort()

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::vector<bool> b;
  b.push_back(false);
  b.push_back(true);
  b.push_back(false);
  b.push_back(true);
  std::sort(b.begin(), b.end());
  VERIFY( b[0] == false && b[1] == false && b[2] == true && b[3] == true );
}

int
main()
{
  test01();
  return 0;
}

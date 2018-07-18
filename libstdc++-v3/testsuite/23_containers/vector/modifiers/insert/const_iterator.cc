// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <vector>

void test01()
{
  std::vector<int> v1, v2{5, 6};
  int n = 0;
  std::vector<int>::iterator it = v1.insert(v1.cbegin(), n);
  it = v1.insert(v1.cbegin(), 1);
  it = v1.insert(v1.cbegin(), {2, 3});
  it = v1.insert(v1.cbegin(), 1, 4);
  it = v1.insert(v1.cbegin(), v2.begin(), v2.end());
}

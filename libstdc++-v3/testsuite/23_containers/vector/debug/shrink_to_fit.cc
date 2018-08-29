// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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
//
// { dg-do run { target c++11 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <vector>

void test01()
{
  using std::vector;
  vector<int> v;
  v.reserve(2);
  v.push_back(0);
  vector<int>::iterator it = v.begin();
  v.shrink_to_fit();
  // Following line should assert
  *it = 1;
}

int main()
{
  test01();
  return 0;
}

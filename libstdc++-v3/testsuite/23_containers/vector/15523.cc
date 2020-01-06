// Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

// NB: This issue affected only debug-mode.

#include <vector> 

// libstdc++/15523
void test01() 
{
  using namespace std;

  vector<vector<int>::const_iterator> x(2); 

  vector<int>::iterator i2, i3;
  vector<int>::const_iterator ci1(i2);

  i2 = i3;
} 

int main()
{
  test01();
  return 0;
}

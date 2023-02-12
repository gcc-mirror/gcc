// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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
  std::vector<bool> vb1, vb2{true, false};
  std::vector<bool>::iterator it = vb1.insert(vb1.cbegin(), true);
  it = vb1.insert(vb1.cbegin(), {false, true});
  it = vb1.insert(vb1.cbegin(), 1, false);
  it = vb1.insert(vb1.cbegin(), vb2.begin(), vb2.end());  
}

// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do compile { target c++11 } }
// { dg-skip-if "" { *-*-* } { "-D_GLIBCXX_PARALLEL" } }

// PR libstdc++/84998

#include <bitset>
#include <vector>

std::size_t
test01()
{
  std::bitset<1> b;
  std::hash<std::bitset<1>> h;
  return h(b);
}

std::size_t
test02()
{
  std::vector<bool> b;
  std::hash<std::vector<bool>> h;
  return h(b);
}

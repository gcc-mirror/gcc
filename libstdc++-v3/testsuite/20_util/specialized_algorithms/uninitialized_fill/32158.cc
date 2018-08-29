// Copyright (C) 2007-2018 Free Software Foundation, Inc.
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

// 20.4.4 specialized algorithms

// { dg-do compile }

#include <memory>
#include <utility>

// c++/32158

typedef std::pair<const int, int> MyPair;

void
Alpha(MyPair* start, MyPair* end)
{
  MyPair my_pair(1, 2);
  std::uninitialized_fill(start, end, my_pair);
}

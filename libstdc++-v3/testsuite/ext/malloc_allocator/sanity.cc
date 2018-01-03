// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <map>
#include <vector>
#include <ext/malloc_allocator.h>

int main()
{
  std::map<int, int, std::less<int>,
	   __gnu_cxx::malloc_allocator<std::pair<int, int> > > allocs;
  allocs[9] = 3;
  std::vector<int, __gnu_cxx::malloc_allocator<int>> vec(10);
  vec[5] = 42;
}

// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-do compile }

#include <algorithm>

void
test01(int* first, int* last)
{
  extern bool cmp(int, int);
  // PR libstdc++/80064
  // This is undefined, because [alg.sorting] requires the template argument
  // Compare to be a function object type, and bool(int, int) is not an
  // object type. We previously accepted it by accident, so keep doing so.
  std::make_heap<int*, bool(int, int)>(first, last, cmp);
}

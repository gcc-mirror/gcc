// Copyright (C) 2017-2022 Free Software Foundation, Inc.
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
// { dg-require-effective-target hosted }

#include <functional>
#include <algorithm>
#include <iterator>

void
test01()
{
  int i[] = { 1, 2, 3, 4 };
  std::function<bool(int, int)> f = std::less<int>{};
  // If this uses a moved-from std::function we'll get an exception:
  std::make_heap(std::begin(i), std::end(i), f);
  std::sort_heap(std::begin(i), std::end(i), f);
}

int
main()
{
  test01();
}

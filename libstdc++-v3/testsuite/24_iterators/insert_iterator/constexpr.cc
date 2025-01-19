// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }
// { dg-add-options no_pch }

#include <iterator>

#ifndef __cpp_lib_constexpr_iterator
# error "Feature test macro for constexpr insert iterators is missing in <iterator>"
#elif __cpp_lib_constexpr_iterator < 201811L
# error "Feature test macro for constexpr insert iterators has wrong value in <iterator>"
#endif

struct container
{
  using value_type = int;

  constexpr int* begin() { return std::begin(data); }
  constexpr int* end() { return last; }

  constexpr int* insert(int* pos, int val)
  {
    if (last == std::end(data))
      throw val;
    for (int* i = last++; i != pos; --i)
      i[1] = i[0];
    *pos = val;
    return pos;
  }

  int data[3];
  int* last = std::begin(data);
};

constexpr bool
test01()
{
  container c;
  std::insert_iterator<container> iter = std::inserter(c, c.begin());
  *iter++ = 1;
  int i = 2;
  *iter = i;
  *++iter = 3;
  return c.data[0] == 1 && c.data[1] == 2 && c.data[2] == 3;
}

static_assert( test01() );

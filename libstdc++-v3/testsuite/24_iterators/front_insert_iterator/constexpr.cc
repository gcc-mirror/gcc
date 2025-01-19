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

#include <iterator>

struct container
{
  using value_type = int;

  constexpr int* begin() { return next; }
  constexpr int* end() { return std::end(data); }

  constexpr void push_front(int val)
  {
    if (next == std::begin(data))
      throw val;
    *--next = val;
  }

  int data[3];
  int* next = std::end(data);
};

constexpr bool
test01()
{
  container c;
  std::front_insert_iterator<container> iter = std::front_inserter(c);
  *iter++ = 1;
  int i = 2;
  *iter = i;
  *++iter = 3;
  return c.data[0] == 3 && c.data[1] == 2 && c.data[2] == 1;
}

static_assert( test01() );

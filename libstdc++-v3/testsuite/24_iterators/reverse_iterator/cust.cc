// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

// This test is an adaptation of 24_iterators/move_iterator/cust.cc.

constexpr bool
test01()
{
  struct X
  {
    constexpr X(int i) noexcept : i(i) { }
    constexpr X(X&& x) noexcept : i(x.i) { x.i = -1; }
    constexpr X& operator=(X&& x) noexcept { i = x.i; x.i = 0; return *this; }
    int i;
  };

  X arr[] = { 1, 2 };
  std::reverse_iterator<X*> i(arr + 1), j(arr + 2);
  static_assert(noexcept(std::ranges::iter_swap(i, j)));
  std::ranges::iter_swap(i, j);
  VERIFY( arr[0].i == 2 );
  VERIFY( arr[1].i == 1 );

  static_assert(noexcept(std::ranges::iter_move(i)));
  X x = std::ranges::iter_move(i);
  VERIFY( arr[0].i == -1 );
  VERIFY( x.i == 2 );

  return true;
}

static_assert(test01());

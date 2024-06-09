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

// { dg-do run { target c++20 } }

#include <iterator>
#include <testsuite_hooks.h>

void
test01()
{
  struct X
  {
    X(int i) : i(i) { }
    X(X&& x) : i(x.i) { x.i = -1; }
    X& operator=(X&& x) { i = x.i; x.i = 0; return *this; }
    int i;
  };

  X arr[] = { 1, 2 };
  std::move_iterator<X*> i(arr), j(arr + 1);
  std::ranges::iter_swap(i, j);
  VERIFY( arr[0].i == 2 );
  VERIFY( arr[1].i == 1 );

  X x = std::ranges::iter_move(i);
  VERIFY( arr[0].i == -1 );
  VERIFY( x.i == 2 );
}

int
main()
{
  test01();
}

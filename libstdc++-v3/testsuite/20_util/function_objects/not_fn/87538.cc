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

// { dg-do run { target c++17 } }

#include <functional>
#include <testsuite_hooks.h>

struct N {
  int operator()(int i) { if (i == 0) throw -1; return i; }
};

void
test01()
{
  N n;
  auto not_n = std::not_fn(n);
  static_assert( !noexcept(not_n(1)) );
  VERIFY(not_n(1) == 0);
  int exception = 0;
  try {
    not_n(0);
  }
  catch (int e) {
    exception = e;
  }
  VERIFY(exception == -1);
}

int
main()
{
  test01();
}

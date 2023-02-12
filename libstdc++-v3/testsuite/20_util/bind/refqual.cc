// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

#include <functional>
#include <testsuite_hooks.h>

struct X
{
  int f() const& noexcept { return 0; }
  int g(int i, ...)& noexcept { return i; }
};

void
test01()
{
  X x;
  auto b = std::bind(&X::f, &x);
  VERIFY( b() == 0 );
  auto bb = std::bind(&X::g, &x, 1, 2);
  VERIFY( bb() == 1 );

  // Check for weak result types:
  using T1 = decltype(b)::result_type;
  using T2 = decltype(bb)::result_type;
}

int
main()
{
  test01();
}

// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#include <variant>
#include <testsuite_hooks.h>

// PR libstdc++/89851

void
test01()
{
  using V = std::variant<int, int>;
  V v1{std::in_place_index<0>, 0};
  V v2{std::in_place_index<1>, 0};
  VERIFY(   v1 != v2  );
  VERIFY( !(v1 == v2) );
}

int
main()
{
  test01();
}

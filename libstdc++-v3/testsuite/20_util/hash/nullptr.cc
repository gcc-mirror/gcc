// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

void
test01()
{
  auto h1 = std::hash<std::nullptr_t>{}(nullptr);
  static_assert(std::is_same_v<decltype(h1), std::size_t>);
  auto h2 = std::hash<std::nullptr_t>{}(nullptr);
  VERIFY( h1 == h2 );
}

int
main()
{
  test01();
}

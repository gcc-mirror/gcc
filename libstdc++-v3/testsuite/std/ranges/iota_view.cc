// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <ranges>
#include <testsuite_hooks.h>

void
test01()
{
  int vals[5] = { };
  int* out = vals;
  for (int i : std::ranges::iota_view{1, 4})
    *out++ = i;
  VERIFY(out == vals + 3);
  VERIFY(vals[0] == 1);
  VERIFY(vals[1] == 2);
  VERIFY(vals[2] == 3);
  VERIFY(vals[3] == 0);
}

void
test02()
{
  auto v = std::ranges::views::iota(4);
  auto it = v.begin();
  VERIFY( *it == 4 );
  ++it;
  VERIFY( *it == 5 );
  it++;
  VERIFY( *it == 6 );
}

void
test03()
{
  auto v = std::ranges::views::iota(10, 15);
  auto it = v.begin();
  VERIFY( *it == 10 );
  it += 2;
  VERIFY( *it == 12 );
  it += 2;
  VERIFY( *it == 14 );
  ++it;
  VERIFY( it == v.end() );
}

int
main()
{
  test01();
  test02();
  test03();
}

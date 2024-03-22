// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

struct MoveOnly
{
  MoveOnly(int i) : i(i) { }
  MoveOnly(MoveOnly&&) = default;
  int i;
};

void
test01()
{
  char buf[sizeof(MoveOnly)*2];
  MoveOnly* addr = (MoveOnly*)buf;
  std::raw_storage_iterator<MoveOnly*, MoveOnly> iter(addr); // { dg-warning "is deprecated" "" { target c++17 } }
  *iter++ = MoveOnly{1};
  *iter++ = MoveOnly{2};
  VERIFY( addr[0].i == 1 );
  VERIFY( addr[1].i == 2 );
}

int
main()
{
  test01();
}

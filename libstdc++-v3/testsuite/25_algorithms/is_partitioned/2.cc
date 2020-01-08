// Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

#include <algorithm>
#include <testsuite_hooks.h>

// PR libstdc++/64903

int count;

struct pred
{
  bool operator()(int i) const { ++count; return i < 5; }
};

void
test01()
{
  int i[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  bool result = std::is_partitioned(i, i + 10, pred());
  VERIFY( result );
  VERIFY( count == 10 );
}

int
main()
{
  test01();
}

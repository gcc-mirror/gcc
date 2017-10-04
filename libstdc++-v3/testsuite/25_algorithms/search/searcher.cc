// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <algorithm>
#include <testsuite_hooks.h>

struct nocopy
{
  nocopy() = default;
  nocopy(const nocopy&) = delete;
  nocopy& operator=(const nocopy&) = delete;

  struct P { int* first; };

  P operator()(int* f, int* l) const { return {f}; }
};

void
test01()
{
  int i[] = { 1, 2 };
  auto res = std::search(i, i + 2, nocopy{});
  VERIFY( res == i );
}

int
main()
{
  test01();
}

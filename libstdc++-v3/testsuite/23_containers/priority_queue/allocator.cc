// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

#include <queue>
#include <testsuite_hooks.h>

// PR libstdc++/70101

struct comp
{
  int init_check[64];
  bool operator()(int l, int r) const { return l < r; }
};

struct check_init : std::priority_queue<int, std::vector<int>, comp>
{
  template<typename... Args>
    check_init(Args... args) : priority_queue(args...)
    {
      push(0);

      for (int i : comp.init_check)
        VERIFY( i == 0 );  // Will not fail if *this was value-initialized.
    }
};

void
test01()
{
  std::vector<int> vec;
  comp cmp{};
  std::allocator<int> alloc;
  std::priority_queue<int, std::vector<int>, comp> pq;
  check_init c1( pq, alloc );
  check_init c2( std::move(pq), alloc );
  check_init c3( alloc );
  check_init c4( cmp, alloc );
  check_init c5( cmp, vec, alloc );
  check_init c6( cmp, std::move(vec), alloc );
}

int
main()
{
  test01();
}

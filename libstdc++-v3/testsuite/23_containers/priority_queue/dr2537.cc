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

// { dg-do run { target c++11 } }

#include <queue>
#include <testsuite_hooks.h>

struct Q : std::priority_queue<int>
{
  using priority_queue::priority_queue;

  bool is_heap() const
  { return std::is_heap(c.begin(), c.end()); }
};

void
test01()
{
  const Q::value_compare cmp;
  const Q::container_type c{ 2, 3, 5, 7, 11, 13, 17, 19, 23 };
  const Q::container_type::allocator_type a;

  Q q1(cmp, c, a);
  VERIFY( q1.is_heap() );

  auto c2 = c;
  Q q2(cmp, std::move(c2), a);
  VERIFY( q2.is_heap() );
}

int
main()
{
  test01();
}

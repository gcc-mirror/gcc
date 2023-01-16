// Copyright (C) 2021-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <queue>

#ifndef __cpp_lib_adaptor_iterator_pair_constructor
#error Feature test macro for iterator pair constructors is missing in <queue>
#elif __cpp_lib_adaptor_iterator_pair_constructor != 202106L
#error Feature test macro for iterator pair constructors has wrong value in <queue>
#endif

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

void
test_p1425r4()
{
  const int vals[] = { 5, 4, 3, 2, 1, 9 };

  std::queue<int> q(std::begin(vals), std::end(vals));
  VERIFY( q.size() == std::size(vals) );
  VERIFY( q.front() == 5 );
  VERIFY( q.back() == 9 );

  using Alloc = __gnu_test::uneq_allocator<int>;

  struct Queue : std::queue<int, std::deque<int, Alloc>>
  {
    using queue::queue;

    Alloc get_allocator() const { return c.get_allocator(); }
  };

  Alloc a0, a1(1);
  Queue q0(std::next(vals), std::end(vals));
  VERIFY( q0.size() == std::size(vals) - 1 );
  VERIFY( q0.front() == 4 );
  VERIFY( q0.back() == 9 );
  VERIFY( q0.get_allocator() == a0 );

  Queue q1(std::next(vals, 2), std::end(vals), a1);
  VERIFY( q1.size() == std::size(vals) - 2 );
  VERIFY( q1.front() == 3 );
  VERIFY( q1.back() == 9 );
  VERIFY( q1.get_allocator() == a1 );
}

int main()
{
  test_p1425r4();
}

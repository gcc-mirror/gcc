// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++23 } }
// { dg-add-options no_pch }

#include <stack>

#ifndef __cpp_lib_adaptor_iterator_pair_constructor
#error Feature test macro for iterator pair constructors is missing in <stack>
#elif __cpp_lib_adaptor_iterator_pair_constructor != 202106L
#error Feature test macro for iterator pair constructors has wrong value in <stack>
#endif

#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

void
test_p1425r4()
{
  const int vals[] = { 1, 2, 3, 7, 8, 9, 5, 6, 7 };

  std::stack<int> s(std::begin(vals), std::end(vals));
  VERIFY( s.size() == std::size(vals) );
  VERIFY( s.top() == 7 );

  using Alloc = __gnu_test::uneq_allocator<int>;

  struct Stack : std::stack<int, std::deque<int, Alloc>>
  {
    using stack::stack;

    Alloc get_allocator() const { return c.get_allocator(); }
  };

  Alloc a0, a1(1);
  Stack s0(std::begin(vals), std::end(vals) - 1);
  VERIFY( s0.size() == std::size(vals) - 1 );
  VERIFY( s0.top() == 6 );
  VERIFY( s0.get_allocator() == a0 );

  Stack s1(std::begin(vals), std::end(vals) - 2, a1);
  VERIFY( s1.size() == std::size(vals) - 2 );
  VERIFY( s1.top() == 5 );
  VERIFY( s1.get_allocator() == a1 );
}

int main()
{
  test_p1425r4();
}

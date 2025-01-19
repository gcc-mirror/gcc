// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#include <forward_list>
#include <debug/forward_list>
#include <testsuite_hooks.h>

template<typename Container>
void
test(std::initializer_list<typename Container::value_type> vals)
{
  Container c{vals};
  c = std::move(c);
  VERIFY( c == c );

  auto it = c.begin();
  it = std::move(it);
  VERIFY( it == c.begin() );
}

int
main()
{
  test<std::forward_list<int>>({1, 2, 3});
  test<std::forward_list<std::forward_list<int>>>({{1,2}, {3,4}, {5,6}, {7,8}});
  test<__gnu_debug::forward_list<int>>({1, 2, 3});
  test<__gnu_debug::forward_list<std::forward_list<int>>>({{1,2}, {3,4}});
}

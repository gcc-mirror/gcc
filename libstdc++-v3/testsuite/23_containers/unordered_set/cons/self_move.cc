// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <unordered_set>
#include <debug/unordered_set>
#include <string>
#include <stdlib.h>
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

  auto localit = c.begin(0);
  localit = std::move(localit);
}

int
main()
{
  std::string s = "how long is a piece of SSO string?";
  test<std::unordered_set<int>>({1, 2, 3});
  test<std::unordered_set<std::string>>({s, s, s, s});
  test<__gnu_debug::unordered_set<int>>({1, 2, 3});
  test<__gnu_debug::unordered_set<std::string>>({s, s, s, s});
}

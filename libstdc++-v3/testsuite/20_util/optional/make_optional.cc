// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 }  }

// Copyright (C) 2013-2019 Free Software Foundation, Inc.
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

#include <optional>
#include <testsuite_hooks.h>
#include <vector>
#include <tuple>

struct combined {
  std::vector<int> v;
  std::tuple<int, int> t;
  template<class... Args>
  combined(std::initializer_list<int> il, Args&&... args)
    : v(il), t(std::forward<Args>(args)...)
  {
  }
};

int main()
{
  const int i = 42;
  auto o = std::make_optional(i);
  static_assert( std::is_same<decltype(o), std::optional<int>>(), "" );
  VERIFY( o && *o == 42 );
  VERIFY( &*o != &i );
  auto o2 = std::make_optional<std::tuple<int, int>>(1, 2);
  static_assert( std::is_same<decltype(o2),
		 std::optional<std::tuple<int, int>>>(), "" );
  VERIFY( o2 && std::get<0>(*o2) == 1 && std::get<1>(*o2) == 2);
  auto o3 = std::make_optional<std::vector<int>>({42, 666});
  static_assert( std::is_same<decltype(o3),
		 std::optional<std::vector<int>>>(), "" );
  VERIFY(o3 && (*o3)[0] == 42 && (*o3)[1] == 666);
  auto o4 = std::make_optional<combined>({42, 666});
  static_assert( std::is_same<decltype(o4),
		 std::optional<combined>>(), "" );
  VERIFY(o4 && (o4->v)[0] == 42 && (o4->v)[1] == 666
	 && std::get<0>(o4->t) == 0 && std::get<1>(o4->t) == 0 );
  auto o5 = std::make_optional<combined>({1, 2}, 3, 4);
  static_assert( std::is_same<decltype(o5),
		 std::optional<combined>>(), "" );
  VERIFY(o4 && (o5->v)[0] == 1 && (o5->v)[1] == 2
	 && std::get<0>(o5->t) == 3 && std::get<1>(o5->t) == 4 );
}

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

// { dg-do compile { target c++20 } }

#include <compare>

template<typename C, typename T, typename U>
  concept comparable = requires (const C& cmp, const T& t, const U& u) {
    cmp(t, u);
  };

void
test01()
{
  struct X
  {
    operator int() const { return 0; }
    operator long*() const { return nullptr; }
  } x;

  // Not three-way-comparable because of ambiguous conversion to int or long*:
  static_assert( ! std::three_way_comparable<X> );

  // And therefore X is not three-way-comparable-with anything else
  // (because std::three_way_comparable_with<X, Y> requires that both
  // three_way_comparable<X> and three_way_comparable<Y> are true).
  static_assert( ! std::three_way_comparable_with<X, long*> );

  long l;
  // But <=> is valid and resolves to a builtin operator comparing pointers:
  [[maybe_unused]] auto c = &l <=> x;

  // But LWG 3530 says std::compare_three_way should not be usable:
  static_assert( ! comparable<std::compare_three_way, long*, X> );
  static_assert( ! comparable<std::compare_three_way, X, long*> );
}

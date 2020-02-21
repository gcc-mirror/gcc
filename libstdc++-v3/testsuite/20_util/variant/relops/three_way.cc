// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <variant>

void
test01()
{
  using V = std::variant<int, int>;
  constexpr auto I0 = std::in_place_index<0>;
  constexpr auto I1 = std::in_place_index<1>;

  static_assert( std::is_eq(V{I0, 0} <=> V{I0, 0})  );
  static_assert( std::is_eq(V{I0, 1} <=> V{I0, 1})  );

  static_assert( std::is_lt(V{I0, 0} <=> V{I1, 0})  );
  static_assert( std::is_lt(V{I0, 1} <=> V{I1, 0})  );

  static_assert( std::is_gt(V{I0, 1} <=> V{I0, 0})  );
  static_assert( std::is_gt(V{I1, 0} <=> V{I0, 1})  );

  static_assert( V{I0, 0} == V{I0, 0}  );
  static_assert( V{I0, 0} != V{I1, 0}  );
  static_assert( V{I1, 0} != V{I1, 1}  );
}

void
test02()
{
  static_assert( std::is_eq(std::monostate{} <=> std::monostate{}) );
  static_assert( std::monostate{} == std::monostate{} );
  static_assert( std::monostate{} <= std::monostate{} );
  static_assert( std::monostate{} >= std::monostate{} );
  static_assert( !(std::monostate{} != std::monostate{}) );
  static_assert( !(std::monostate{} < std::monostate{}) );
  static_assert( !(std::monostate{} > std::monostate{}) );
}

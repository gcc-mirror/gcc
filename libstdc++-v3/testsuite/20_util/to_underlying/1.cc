// Copyright (C) 2021-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <utility>

#ifndef __cpp_lib_to_underlying
# error "Feature-test macro for to_underlying missing in <utility>"
#elif __cpp_lib_to_underlying != 202102L
# error "Feature-test macro for to_underlying has wrong value in <utility>"
#endif

void
test01()
{
  enum E : short { e0, e1, e2 };
  E e = e0;
  static_assert( std::is_same_v<decltype(std::to_underlying(e)), short> );
  static_assert( noexcept(std::to_underlying(e)) );
  static_assert( std::to_underlying(e1) == 1 );
  static_assert( std::to_underlying((E)3) == 3 );
}

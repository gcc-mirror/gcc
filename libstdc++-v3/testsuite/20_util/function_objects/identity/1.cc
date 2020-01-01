// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <functional>

// C++20 [func.identity]
static_assert( std::is_default_constructible_v<std::identity> );
static_assert( std::is_copy_constructible_v<std::identity> );
static_assert( std::is_move_constructible_v<std::identity> );
static_assert( std::is_copy_assignable_v<std::identity> );
static_assert( std::is_move_assignable_v<std::identity> );

static_assert( !std::is_invocable_v<std::identity> );
static_assert( !std::is_invocable_v<std::identity, int&, int&> );
static_assert( std::is_nothrow_invocable_r_v<int&, std::identity&, int&> );
static_assert( std::is_nothrow_invocable_r_v<const long&, std::identity, const long&> );
static_assert( std::is_nothrow_invocable_r_v<short&&, const std::identity&, short> );
static_assert( std::is_nothrow_invocable_r_v<const char&&, const std::identity, const char> );

int i;
static_assert( std::addressof(std::identity{}(i)) == std::addressof(i) );

using T = std::identity::is_transparent; // required typedef

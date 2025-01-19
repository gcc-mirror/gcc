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

// { dg-do compile { target c++17 } }

#include <optional>

template<bool B>
struct X
{
  X() noexcept(B);
  X(const X&) noexcept(B);

  X(int) noexcept(B);
  X(std::initializer_list<int>, int) noexcept(B);

  X(const X<!B>&) noexcept(B);

  X& operator=(const X&) noexcept(false);
};

using std::is_nothrow_constructible_v;
using std::in_place_t;

using Xyes = X<true>;
using Xno = X<false>;
using Oyes = std::optional<Xyes>;
using Ono = std::optional<Xno>;

static_assert( is_nothrow_constructible_v<Oyes> );
static_assert( is_nothrow_constructible_v<Oyes, std::nullopt_t> );
static_assert( is_nothrow_constructible_v<Oyes, const Xyes&> );
static_assert( is_nothrow_constructible_v<Oyes, Xyes> );
static_assert( is_nothrow_constructible_v<Oyes, in_place_t, short> );
static_assert( is_nothrow_constructible_v<Oyes, in_place_t,
						std::initializer_list<int>,
						long> );
static_assert( is_nothrow_constructible_v<Oyes, const Ono&> );
static_assert( is_nothrow_constructible_v<Oyes, Ono> );

static_assert( is_nothrow_constructible_v<Ono> );
static_assert( is_nothrow_constructible_v<Ono, std::nullopt_t> );
static_assert( ! is_nothrow_constructible_v<Ono, const Xno&> );
static_assert( ! is_nothrow_constructible_v<Ono, Xno> );
static_assert( ! is_nothrow_constructible_v<Ono, in_place_t, short> );
static_assert( ! is_nothrow_constructible_v<Ono, in_place_t,
						 std::initializer_list<int>,
						 long> );
static_assert( ! is_nothrow_constructible_v<Ono, const Xyes&> );
static_assert( ! is_nothrow_constructible_v<Ono, Xyes> );

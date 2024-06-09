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

// { dg-do compile { target c++17 } }

#include <optional>

template<bool B>
struct X
{
  X() noexcept(B);

  X(const X&) noexcept(B);
  X& operator=(const X&) noexcept(B);

  X(int) noexcept(B);
  X& operator=(int) noexcept(false);

  X(void*) noexcept(true);
  X& operator=(void*) noexcept(B);

  X(const X<!B>&) noexcept(B);
  X& operator=(const X<!B>&) noexcept(B);

  X(std::initializer_list<int>, int) noexcept(B);
};

using std::is_nothrow_assignable_v;

using Xyes = X<true>;
using Xno = X<false>;
using Oyes = std::optional<Xyes>;
using Ono = std::optional<Xno>;

static_assert( is_nothrow_assignable_v<Oyes, std::nullopt_t> );
static_assert( is_nothrow_assignable_v<Oyes, const Xyes&> );
static_assert( is_nothrow_assignable_v<Oyes, Xyes> );
static_assert( ! is_nothrow_assignable_v<Oyes, int> );
static_assert( is_nothrow_assignable_v<Oyes, void*> );
static_assert( is_nothrow_assignable_v<Oyes, const Ono&> );
static_assert( is_nothrow_assignable_v<Oyes, Ono> );

static_assert( is_nothrow_assignable_v<Ono, std::nullopt_t> );
static_assert( ! is_nothrow_assignable_v<Ono, const Xno&> );
static_assert( ! is_nothrow_assignable_v<Ono, Xno> );
static_assert( ! is_nothrow_assignable_v<Ono, int> );
static_assert( ! is_nothrow_assignable_v<Ono, void*> );
static_assert( ! is_nothrow_assignable_v<Ono, const Xyes&> );
static_assert( ! is_nothrow_assignable_v<Ono, Xyes> );

Xyes xyes;
Xno xno;
Oyes oyes;
Ono ono;
static_assert( noexcept(oyes.emplace()) );
static_assert( noexcept(oyes.emplace(xyes)) );
static_assert( noexcept(oyes.emplace(1)) );
static_assert( noexcept(oyes.emplace(nullptr)) );
static_assert( noexcept(oyes.emplace(xno)) );
static_assert( noexcept(oyes.emplace({1,2,3}, 1)) );

static_assert( ! noexcept(ono.emplace()) );
static_assert( ! noexcept(ono.emplace(xno)) );
static_assert( ! noexcept(ono.emplace(1)) );
static_assert( noexcept(ono.emplace(nullptr)) );
static_assert( ! noexcept(ono.emplace(xyes)) );
static_assert( ! noexcept(ono.emplace({1,2,3}, 1)) );

// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#include <concepts>

static_assert( std::semiregular<int> );
static_assert( std::semiregular<int*> );
static_assert( ! std::semiregular<int&> );
static_assert( ! std::semiregular<void> );
static_assert( ! std::semiregular<void()> );
static_assert( ! std::semiregular<void() noexcept> );
static_assert( ! std::semiregular<void() const> );

struct Trivial { };
static_assert( std::semiregular<Trivial> );

struct NotTrivial
{
  NotTrivial() { }
  ~NotTrivial() { }
};
static_assert( std::semiregular<NotTrivial> );

struct NotDefaultConstructible
{
  NotDefaultConstructible(int) { }
};
static_assert( ! std::semiregular<NotDefaultConstructible> );

struct HasReference
{
  int& ref;
};
static_assert( ! std::semiregular<HasReference> );

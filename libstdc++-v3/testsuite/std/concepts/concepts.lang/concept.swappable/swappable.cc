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

namespace nu
{
  struct S { bool swapped = false; };
  constexpr void swap(S&, S&) { }
  struct T { int i; };

  union U { char c; int i; };
  constexpr void swap(U&, U&) { }
}

static_assert( std::swappable<nu::S> );
static_assert( !std::swappable<const nu::S> );
static_assert( std::swappable<nu::T> );
static_assert( !std::swappable<const nu::T> );
static_assert( std::swappable<nu::U> );
static_assert( !std::swappable<const nu::U> );

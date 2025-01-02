// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#include <concepts>

static_assert( !std::derived_from<int, int> );
static_assert( !std::derived_from<int&, int> );
static_assert( !std::derived_from<int, int&> );
static_assert( !std::derived_from<int&&, int&> );
static_assert( !std::derived_from<const int, int> );
static_assert( !std::derived_from<const int, const int> );

struct A { };
static_assert( std::derived_from<A, A> );
static_assert( std::derived_from<A, const A> );
static_assert( std::derived_from<A const, const A> );
static_assert( std::derived_from<volatile A, const A> );

struct B : A { };
static_assert( !std::derived_from<A, B> );
static_assert( std::derived_from<B, A> );
static_assert( std::derived_from<const B, A> );
static_assert( std::derived_from<B, const A> );
static_assert( std::derived_from<volatile B, const A> );

struct C : private A { };
static_assert( !std::derived_from<A, C> );
static_assert( !std::derived_from<C, A> );

struct D : A { };
struct E : B, D { };
static_assert( !std::derived_from<A, E> );
static_assert( !std::derived_from<E, A> );

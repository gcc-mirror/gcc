// Copyright (C) 2019 Free Software Foundation, Inc.
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

static_assert( std::convertible_to<int, int> );
static_assert( std::convertible_to<int&, int> );
static_assert( !std::convertible_to<int, int&> );
static_assert( std::convertible_to<int, const int&> );
static_assert( !std::convertible_to<int&&, int&> );
static_assert( std::convertible_to<int&&, const int&> );
static_assert( std::convertible_to<const int, int> );
static_assert( !std::convertible_to<const int, int&> );
static_assert( std::convertible_to<const int, const int> );
static_assert( std::convertible_to<const int, const int&> );

static_assert( std::convertible_to<int, float> );
static_assert( !std::convertible_to<int, float&> );
static_assert( !std::convertible_to<int, int*> );
static_assert( std::convertible_to<int*, void*> );
static_assert( std::convertible_to<int*, const void*> );
static_assert( !std::convertible_to<const int*, void*> );
static_assert( !std::convertible_to<int, void> );

struct A { };
static_assert( std::convertible_to<A, A> );
static_assert( std::convertible_to<A, const A> );
static_assert( std::convertible_to<A const, const A> );
static_assert( !std::convertible_to<volatile A, const A> );
static_assert( !std::convertible_to<volatile A, const volatile A&> );
static_assert( std::convertible_to<volatile A&, const volatile A&> );

struct B : A { };
static_assert( !std::convertible_to<A, B> );
static_assert( std::convertible_to<B, A> );
static_assert( std::convertible_to<const B, A> );
static_assert( std::convertible_to<B, const A> );
static_assert( std::convertible_to<B, const A&> );
static_assert( !std::convertible_to<volatile B, const A> );
static_assert( !std::convertible_to<volatile B, const A&> );

struct C : private A { };
static_assert( !std::convertible_to<A, C> );
static_assert( !std::convertible_to<C, A> );

struct D : A { };
struct E : B, D { };
static_assert( !std::convertible_to<A, E> );
static_assert( !std::convertible_to<E, A> );

struct F
{
  F(A) { }
  F(C&&) { }
};
static_assert( std::convertible_to<A, F> );
static_assert( std::convertible_to<const A, F> );
static_assert( std::convertible_to<C, F> );
static_assert( !std::convertible_to<C&, F> );
static_assert( !std::convertible_to<const C, F> );

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

static_assert( std::common_with<int, int> );
static_assert( std::common_with<int, const int> );
static_assert( std::common_with<int&&, const int&> );
static_assert( std::common_with<int&, const int&&> );
static_assert( std::common_with<void, void> );
static_assert( ! std::common_with<int, void> );
static_assert( ! std::common_with<int, int*> );
static_assert( ! std::common_with<int, int()> );

static_assert( std::common_with<int, short> );
static_assert( std::common_with<short, int> );
static_assert( std::common_with<void*, const int*> );

struct A { A(int) { } };
static_assert( std::common_with<A, int> );

struct B { };
static_assert( ! std::common_with<A, B> );

struct C { C(const A&) { } };
static_assert( std::common_with<A, C> );
static_assert( std::common_with<A, const C> );
static_assert( std::common_with<const A, C> );
static_assert( std::common_with<const A, const C> );

struct D;
struct E { E(const D&) { } };
struct D { D(const E&) { } };
static_assert( ! std::common_with<D, E> ); // ambiguous conversion

struct F { };
struct G { };
struct H {
  H(const F&) { }
  H(const G&) { }
};
namespace std
{
  template<> struct common_type<F, G> { using type = H; };
  template<> struct common_type<G, F> { using type = H; };
}

static_assert( std::common_with<F, G> );
static_assert( std::common_with<F, const G> );
static_assert( std::common_with<const F, G> );
static_assert( std::common_with<const F, const G> );

struct Base { };
struct Derived : Base { };
static_assert( std::common_with<Derived, Base> );
static_assert( std::common_with<Derived*, Base*> );

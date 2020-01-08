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

#include <concepts>

static_assert( std::common_reference_with<int, int> );
static_assert( std::common_reference_with<int, const int> );
static_assert( std::common_reference_with<int&&, const int&> );
static_assert( std::common_reference_with<int&, const int&&> );
static_assert( ! std::common_reference_with<int, void> );
static_assert( ! std::common_reference_with<int, int*> );
static_assert( ! std::common_reference_with<int, int()> );

struct A { A(int) { } };
static_assert( std::common_reference_with<A, int> );

struct B { };
static_assert( ! std::common_reference_with<A, B> );

struct C { C(A&) { } };
static_assert( std::common_reference_with<A&, C> );
static_assert( std::common_reference_with<A&, C&&> );
static_assert( std::common_reference_with<A&, const C&> );
static_assert( std::common_reference_with<A&, C&> );
static_assert( ! std::common_reference_with<const A&, C> );
static_assert( ! std::common_reference_with<const A&, const C&> );

struct D;
struct E { E(D&) { } };
struct D { D(E&) { } };
static_assert( ! std::common_reference_with<D&, E&> ); // ambiguous conversion

struct F;
struct G { G(const F&) { } };
struct F { F(const G&) { } };
namespace std
{
  template<template<typename> class Qual1, template<typename> class Qual2>
    struct basic_common_reference<F, G, Qual1, Qual2>
    { using type = Qual1<Qual2<F>>; };
  template<template<typename> class Qual1, template<typename> class Qual2>
    struct basic_common_reference<G, F, Qual1, Qual2>
    { using type = Qual1<Qual2<F>>; };
}

static_assert( ! std::common_reference_with<F&, G&> );
static_assert( std::common_reference_with<F, G> );
static_assert( std::common_reference_with<F, const G> );
static_assert( std::common_reference_with<const F, const G> );

struct Base { };
struct Derived : Base { };
static_assert( std::common_reference_with<Derived&, Base&> );
static_assert( std::common_with<Derived*, Base*> );

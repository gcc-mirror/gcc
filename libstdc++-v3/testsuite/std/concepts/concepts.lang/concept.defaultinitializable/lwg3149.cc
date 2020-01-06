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

// Default-initialization of const T is only valid for class types that are
// const-default-constructible.
static_assert( !std::default_initializable<const int> );
static_assert( !std::default_initializable<const int[1]> );
struct A { int i; };
static_assert( !std::default_initializable<const A> );
static_assert( !std::default_initializable<const A[1]> );
struct B { int i; long l; };
static_assert( !std::default_initializable<const B> );
static_assert( !std::default_initializable<const B[1]> );
struct C : A { };
static_assert( !std::default_initializable<const C> );
static_assert( !std::default_initializable<const C[1]> );
struct D { A a; };
static_assert( !std::default_initializable<const D> );
static_assert( !std::default_initializable<const D[1]> );

struct S0 { explicit S0() = default; };
struct S1 { S0 x; }; // Note: aggregate
// S1{} would be ill-formed, due to copy-list-initialization of S1::x from {}
static_assert( !std::default_initializable<S1> );

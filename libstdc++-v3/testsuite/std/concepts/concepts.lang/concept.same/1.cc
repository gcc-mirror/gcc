// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

static_assert( std::same_as<int, int> );
static_assert( !std::same_as<int&, int> );
static_assert( !std::same_as<int, int&> );
static_assert( !std::same_as<int&&, int&> );
static_assert( !std::same_as<const int, int> );
static_assert( std::same_as<const int, const int> );

struct A { };
static_assert( std::same_as<A, A> );
static_assert( !std::same_as<A, const A> );
static_assert( std::same_as<A const, const A> );
static_assert( !std::same_as<volatile A, const A> );

struct B : A { };
static_assert( !std::same_as<A, B> );

template<typename T, typename U>
  constexpr int
  check_subsumption()
  { return 0; }

template<typename T, typename U>
  requires std::same_as<T, U>
  constexpr int
  check_subsumption()
  { return 1; }

template<typename T, typename U>
  requires std::same_as<U, T> && std::is_const_v<T>
  constexpr int
  check_subsumption()
  { return 2; }

template<typename T, typename U>
  requires std::same_as<U, T> && std::is_volatile_v<T>
  constexpr int
  check_subsumption()
  { return 3; }

static_assert( check_subsumption<short, long>() == 0 );
static_assert( check_subsumption<unsigned, unsigned>() == 1 );
// These will be ambiguous if same_as<T,U> doesn't subsume same_as<U,T>:
static_assert( check_subsumption<const char, const char>() == 2 );
static_assert( check_subsumption<volatile int, volatile int>() == 3 );

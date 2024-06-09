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

// { dg-do compile { target c++20 } }

#include <ranges>

using S1 = std::ranges::subrange<int*>;
using S2 = std::ranges::subrange<long*, void*>;

static_assert( std::tuple_size_v<S1> == 2 );
static_assert( std::tuple_size_v<S2> == 2 );

static_assert( std::same_as<std::tuple_element_t<0, S1>, int*> );
static_assert( std::same_as<std::tuple_element_t<1, S1>, int*> );
// LWG 3398
static_assert( std::same_as<std::tuple_element_t<0, const S1>, int*> );
static_assert( std::same_as<std::tuple_element_t<1, const S1>, int*> );

static_assert( std::same_as<std::tuple_element_t<0, S2>, long*> );
static_assert( std::same_as<std::tuple_element_t<1, S2>, void*> );
// LWG 3398
static_assert( std::same_as<std::tuple_element_t<0, const S2>, long*> );
static_assert( std::same_as<std::tuple_element_t<1, const S2>, void*> );

S1 s1;
static_assert( std::same_as<decltype(std::get<0>(s1)), int*> );
static_assert( std::same_as<decltype(std::get<1>(s1)), int*> );
const S1 c1;
static_assert( std::same_as<decltype(std::get<0>(c1)), int*> );
static_assert( std::same_as<decltype(std::get<1>(c1)), int*> );
S2 s2;
static_assert( std::same_as<decltype(std::get<0>(s2)), long*> );
static_assert( std::same_as<decltype(std::get<1>(s2)), void*> );
const S2 c2;
static_assert( std::same_as<decltype(std::get<0>(c2)), long*> );
static_assert( std::same_as<decltype(std::get<1>(c2)), void*> );

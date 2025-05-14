// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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
#include <tuple>
#include <utility>

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

std::pair<int*, int*> p1 = s1;
std::pair<long*, void*> p2 = s2;
std::tuple<int*, int*> t1 = s1;
std::tuple<long*, void*> t2 = s2;

std::pair<int*, int*> const& p3 = s1;
std::tuple<int*, int*>&& t3 = s1;

std::pair<int const*, int const*> p4 = s1;
std::tuple<int const*, void*> t4 = s1;

static_assert( !std::convertible_to<std::ranges::subrange<int const*>, std::pair<int*, int*>> );
static_assert( !std::convertible_to<std::ranges::subrange<int const*>, std::tuple<int*, int*>> );

static_assert( !std::convertible_to<std::ranges::subrange<int*>, std::pair<long*, long*>> );
static_assert( !std::convertible_to<std::ranges::subrange<int*>, std::tuple<int, int>> );

struct B {};
struct D : B {};

std::ranges::subrange<D*> sd;
std::pair<D*, D*> p5 = sd;
std::tuple<D const*, D const*> t5 = sd;

static_assert( !std::convertible_to<std::ranges::subrange<D*>, std::pair<B*, B*>> );
static_assert( !std::convertible_to<std::ranges::subrange<B*>, std::tuple<D*, D*>> );

// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#include <ranges>
#include <string_view>

namespace ranges = std::ranges;

auto pred_f(int x) { return x%2 == 0; };
auto pred_l = [] (int x) { return x%2 == 0; };

auto func_f(int x) { return x*x; }
auto func_l = [] (int x) { return x*x; };

using V = ranges::subrange<int*, int*>;
constexpr auto ptr = sizeof(int*);
static_assert(sizeof(V) == 2*ptr);

static_assert(sizeof(ranges::take_view<V>) == 3*ptr);
static_assert(sizeof(ranges::drop_view<V>) == 3*ptr);

static_assert(sizeof(ranges::filter_view<V, decltype(&pred_f)>) == 4*ptr);
static_assert(sizeof(ranges::take_while_view<V, decltype(&pred_f)>) == 3*ptr);
static_assert(sizeof(ranges::drop_while_view<V, decltype(&pred_f)>) == 4*ptr);
static_assert(sizeof(ranges::transform_view<V, decltype(&func_f)>) == 3*ptr);

static_assert(sizeof(ranges::filter_view<V, decltype(pred_l)>) == 3*ptr);
static_assert(sizeof(ranges::take_while_view<V, decltype(pred_l)>) == 2*ptr);
static_assert(sizeof(ranges::drop_while_view<V, decltype(pred_l)>) == 3*ptr);
static_assert(sizeof(ranges::transform_view<V, decltype(func_l)>) == 2*ptr);

static_assert(sizeof(ranges::lazy_split_view<V, std::string_view>) == 4*ptr);

static_assert
 (sizeof(ranges::reverse_view<ranges::filter_view<V, decltype(pred_l)>>) == 3*ptr);

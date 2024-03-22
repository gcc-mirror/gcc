// Copyright (C) 2021-2024 Free Software Foundation, Inc.
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

#include <functional>

struct S {
  constexpr operator int*() const { return nullptr; }
};

void operator!=(S const&, S const&) {}
void operator>=(S const&, S const&) {}

// S can be compared via conversion to int*
static_assert(S{} == S{});
static_assert(S{} <= S{});
// But concept not satisfied because operator!= returns void
static_assert(!std::equality_comparable_with<S,S>);
// But concept not satisfied because operator>= returns void
static_assert(!std::totally_ordered<S>);

template<typename C, typename T>
  concept comparable = requires (const C& cmp, const T& t) { cmp(t, t); };

// LWG 3530 says [range.cmp] comparisons should not work for S
static_assert( ! comparable<std::ranges::equal_to,      S> );
static_assert( ! comparable<std::ranges::not_equal_to,  S> );
static_assert( ! comparable<std::ranges::greater,       S> );
static_assert( ! comparable<std::ranges::less,          S> );
static_assert( ! comparable<std::ranges::greater_equal, S> );
static_assert( ! comparable<std::ranges::less_equal,    S> );

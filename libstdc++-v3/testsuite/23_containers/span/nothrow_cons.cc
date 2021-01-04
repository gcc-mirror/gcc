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

#include <span>

using std::span;
using std::is_nothrow_constructible_v;

static_assert( is_nothrow_constructible_v<span<int>> );
static_assert( is_nothrow_constructible_v<span<int, 0>> );

static_assert( is_nothrow_constructible_v<span<int>, span<int>&> );
static_assert( is_nothrow_constructible_v<span<const int>, span<int>&> );
static_assert( is_nothrow_constructible_v<span<int>, span<int, 1>&> );
static_assert( is_nothrow_constructible_v<span<const int>, span<int, 1>&> );
static_assert( is_nothrow_constructible_v<span<int, 1>, span<int, 1>&> );
static_assert( is_nothrow_constructible_v<span<const int, 1>, span<int, 1>&> );

static_assert( is_nothrow_constructible_v<span<int>, int(&)[1]> );
static_assert( is_nothrow_constructible_v<span<int, 1>, int(&)[1]> );
static_assert( is_nothrow_constructible_v<span<int>, std::array<int, 1>&> );
static_assert( is_nothrow_constructible_v<span<int, 1>, std::array<int, 1>&> );

template<bool>
struct sentinel { int* p; };

template<bool B>
bool operator==(sentinel<B> s, int* p) noexcept { return s.p == p; }

template<bool B>
std::ptrdiff_t operator-(sentinel<B> s, int* p) noexcept(B) { return s.p - p; }

template<bool B>
std::ptrdiff_t operator-(int* p, sentinel<B> s) noexcept { return p - s.p; }

static_assert(std::sized_sentinel_for<sentinel<true>, int*>);
static_assert(std::sized_sentinel_for<sentinel<false>, int*>);

static_assert(is_nothrow_constructible_v<span<int>, int*, std::size_t>);
static_assert(is_nothrow_constructible_v<span<int>, int*, const int*>);
static_assert(is_nothrow_constructible_v<span<int>, int*, sentinel<true>>);
static_assert(!is_nothrow_constructible_v<span<int>, int*, sentinel<false>>);

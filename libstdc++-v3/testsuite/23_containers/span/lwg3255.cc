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

#include <span>
#include <array>

using std::span;
using std::dynamic_extent;
using std::array;
using std::is_constructible_v;

// LWG 3255 span's array constructor is too strict

static_assert( is_constructible_v<span<const int* const>, array<int*, 2>> );
static_assert( is_constructible_v<span<const int>, array<const int, 4>> );

static_assert( is_constructible_v<span<int, 1>, int(&)[1]> );
static_assert( is_constructible_v<span<const int, 1>, int(&)[1]> );
static_assert( is_constructible_v<span<const int, 1>, const int(&)[1]> );

static_assert( is_constructible_v<span<int, 1>, array<int, 1>&> );
static_assert( is_constructible_v<span<const int, 1>, array<int, 1>&> );
static_assert( is_constructible_v<span<const int, 1>, array<const int, 1>&> );

static_assert( is_constructible_v<span<int, 1>, const array<int, 1>&> );
static_assert( is_constructible_v<span<const int, 1>, const array<int, 1>&> );
static_assert( is_constructible_v<span<const int, 1>, const array<const int, 1>&> );

static_assert( !is_constructible_v<span<int, 1>, int(&)[2]> );
static_assert( !is_constructible_v<span<const int, 1>, int(&)[2]> );
static_assert( !is_constructible_v<span<const int, 1>, const int(&)[2]> );

static_assert( !is_constructible_v<span<int, 1>, array<int, 2>&> );
static_assert( !is_constructible_v<span<const int, 1>, array<int, 2>&> );
static_assert( !is_constructible_v<span<const int, 1>, array<const int, 2>&> );

static_assert( !is_constructible_v<span<int, 1>, const array<int, 2>&> );
static_assert( !is_constructible_v<span<const int, 1>, const array<int, 2>&> );
static_assert( !is_constructible_v<span<const int, 1>, const array<const int, 2>&> );

static_assert( is_constructible_v<span<int>, int(&)[2]> );
static_assert( is_constructible_v<span<const int>, int(&)[2]> );
static_assert( is_constructible_v<span<const int>, const int(&)[2]> );

static_assert( is_constructible_v<span<int>, array<int, 2>&> );
static_assert( is_constructible_v<span<const int>, array<int, 2>&> );
static_assert( is_constructible_v<span<const int>, array<const int, 2>&> );

static_assert( is_constructible_v<span<int>, const array<int, 2>&> );
static_assert( is_constructible_v<span<const int>, const array<int, 2>&> );
static_assert( is_constructible_v<span<const int>, const array<const int, 2>&> );

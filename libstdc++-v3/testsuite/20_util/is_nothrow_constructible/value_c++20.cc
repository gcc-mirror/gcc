// { dg-do compile { target c++20 } }

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <type_traits>

static_assert( std::is_nothrow_constructible_v<int[1]> );
static_assert( std::is_nothrow_constructible_v<int[1], int> );
static_assert( std::is_nothrow_constructible_v<int[2], int> );
static_assert( std::is_nothrow_constructible_v<int[2], int, int> );
static_assert( ! std::is_nothrow_constructible_v<int[1], int, int> );
static_assert( ! std::is_nothrow_constructible_v<int[]> );
static_assert( ! std::is_nothrow_constructible_v<int[], int> );
static_assert( ! std::is_nothrow_constructible_v<int[], int, int> );

struct X
{
  X() = default;
  X(int) noexcept { }
  X(double) { }
};

static_assert( std::is_nothrow_constructible_v<X[2]> );
static_assert( std::is_nothrow_constructible_v<X[1], X> );
static_assert( std::is_nothrow_constructible_v<X[1], int> );
static_assert( ! std::is_nothrow_constructible_v<X[1], double> );
static_assert( ! std::is_nothrow_constructible_v<X[2], int, double> );

struct Y
{
  int i;
  X x;
};

static_assert( std::is_nothrow_constructible_v<Y> );
static_assert( std::is_nothrow_constructible_v<Y, Y> );
static_assert( std::is_nothrow_constructible_v<Y, int> );
static_assert( ! std::is_nothrow_constructible_v<Y, X> );
static_assert( std::is_nothrow_constructible_v<Y, int, X> );
static_assert( std::is_nothrow_constructible_v<Y, int, int> );
static_assert( ! std::is_nothrow_constructible_v<Y, int, double> );

struct Z : Y { };

static_assert( std::is_nothrow_constructible_v<Z> );
static_assert( std::is_nothrow_constructible_v<Z, Z> );
static_assert( std::is_nothrow_constructible_v<Z, Y> );
static_assert( ! std::is_nothrow_constructible_v<Z, int> );
static_assert( ! std::is_nothrow_constructible_v<Z, int, X> );
static_assert( ! std::is_nothrow_constructible_v<Z, int, int> );
static_assert( ! std::is_nothrow_constructible_v<Z, Y, double> );
static_assert( ! std::is_nothrow_constructible_v<Z, int, double> );
static_assert( ! std::is_nothrow_constructible_v<Z, X> );

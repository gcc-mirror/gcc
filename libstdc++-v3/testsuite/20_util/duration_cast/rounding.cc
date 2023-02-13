// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <chrono>

#if __cpp_lib_chrono < 201510
# error "__cpp_lib_chrono < 201510"
#endif

using namespace std::chrono_literals;
using std::chrono::seconds;

using fp_seconds = std::chrono::duration<float>;

static_assert( std::chrono::floor<seconds>(1000ms) == 1s );
static_assert( std::chrono::floor<seconds>(1001ms) == 1s );
static_assert( std::chrono::floor<seconds>(1500ms) == 1s );
static_assert( std::chrono::floor<seconds>(1999ms) == 1s );
static_assert( std::chrono::floor<seconds>(2000ms) == 2s );
static_assert( std::chrono::floor<seconds>(2001ms) == 2s );
static_assert( std::chrono::floor<seconds>(2500ms) == 2s );
static_assert( std::chrono::floor<fp_seconds>(500ms) == fp_seconds{0.5f} );

static_assert( std::chrono::ceil<seconds>(1000ms) == 1s );
static_assert( std::chrono::ceil<seconds>(1001ms) == 2s );
static_assert( std::chrono::ceil<seconds>(1500ms) == 2s );
static_assert( std::chrono::ceil<seconds>(1999ms) == 2s );
static_assert( std::chrono::ceil<seconds>(2000ms) == 2s );
static_assert( std::chrono::ceil<seconds>(2001ms) == 3s );
static_assert( std::chrono::ceil<seconds>(2500ms) == 3s );
static_assert( std::chrono::ceil<fp_seconds>(500ms) == fp_seconds{0.5f} );

static_assert( std::chrono::round<seconds>(1000ms) == 1s );
static_assert( std::chrono::round<seconds>(1001ms) == 1s );
static_assert( std::chrono::round<seconds>(1499ms) == 1s );
static_assert( std::chrono::round<seconds>(1500ms) == 2s );
static_assert( std::chrono::round<seconds>(1999ms) == 2s );
static_assert( std::chrono::round<seconds>(2000ms) == 2s );
static_assert( std::chrono::round<seconds>(2001ms) == 2s );
static_assert( std::chrono::round<seconds>(2500ms) == 2s );
static_assert( std::chrono::round<seconds>(2501ms) == 3s );

static_assert( std::chrono::abs(100ms) == 100ms );
static_assert( std::chrono::abs(-100ms) == 100ms );

// LWG 3741. std::chrono::abs(duration) is ill-formed with non-reduced periods
using D1000 = std::chrono::duration<int, std::ratio<1000, 1000>>;
static_assert( std::chrono::abs(D1000(-2)) == D1000(2) );
static_assert( std::is_same_v<decltype(std::chrono::abs(D1000(-2))), D1000> );

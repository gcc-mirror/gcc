// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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
// { dg-add-options no_pch }

#include <chrono>

#if __cpp_lib_chrono < 201510
# error "__cpp_lib_chrono < 201510"
#endif

using namespace std::chrono_literals;
using std::chrono::seconds;
using std::chrono::milliseconds;

constexpr std::chrono::system_clock::time_point base{};

static_assert( std::chrono::floor<seconds>(base + 1000ms) == (base + 1s) );
static_assert( std::chrono::floor<seconds>(base + 1001ms) == (base + 1s) );
static_assert( std::chrono::floor<seconds>(base + 1500ms) == (base + 1s) );
static_assert( std::chrono::floor<seconds>(base + 1999ms) == (base + 1s) );
static_assert( std::chrono::floor<seconds>(base + 2000ms) == (base + 2s) );
static_assert( std::chrono::floor<seconds>(base + 2001ms) == (base + 2s) );
static_assert( std::chrono::floor<seconds>(base + 2500ms) == (base + 2s) );

static_assert( std::chrono::ceil<seconds>(base + 1000ms) == (base + 1s) );
static_assert( std::chrono::ceil<seconds>(base + 1001ms) == (base + 2s) );
static_assert( std::chrono::ceil<seconds>(base + 1500ms) == (base + 2s) );
static_assert( std::chrono::ceil<seconds>(base + 1999ms) == (base + 2s) );
static_assert( std::chrono::ceil<seconds>(base + 2000ms) == (base + 2s) );
static_assert( std::chrono::ceil<seconds>(base + 2001ms) == (base + 3s) );
static_assert( std::chrono::ceil<seconds>(base + 2500ms) == (base + 3s) );

static_assert( std::chrono::round<seconds>(base + 1000ms) == (base + 1s) );
static_assert( std::chrono::round<seconds>(base + 1001ms) == (base + 1s) );
static_assert( std::chrono::round<seconds>(base + 1499ms) == (base + 1s) );
static_assert( std::chrono::round<seconds>(base + 1500ms) == (base + 2s) );
static_assert( std::chrono::round<seconds>(base + 1999ms) == (base + 2s) );
static_assert( std::chrono::round<seconds>(base + 2000ms) == (base + 2s) );
static_assert( std::chrono::round<seconds>(base + 2001ms) == (base + 2s) );
static_assert( std::chrono::round<seconds>(base + 2500ms) == (base + 2s) );
static_assert( std::chrono::round<seconds>(base + 2501ms) == (base + 3s) );

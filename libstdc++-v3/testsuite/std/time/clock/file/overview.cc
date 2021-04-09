// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <chrono>

using std::chrono::file_clock;

// Cpp17Clock requirements:

// New type so that is_clock<file_clock> specialization isn't used.
struct C : file_clock { };
static_assert( std::chrono::is_clock_v<C> );

// Cpp17TrivialClock requirements:

// A trivial clock's rep must be a numeric type, which is true for
// libstdc++ because we use an integral type.
static_assert( std::is_integral_v<file_clock::rep> );

// We meet the recursive Cpp17TrivialClock requirement by using the same clock:
static_assert( std::is_same_v<file_clock::time_point::clock, file_clock> );

// chrono::file_clock requirements:

static_assert( std::is_signed_v<file_clock::rep> );
static_assert( noexcept(file_clock::now()) );

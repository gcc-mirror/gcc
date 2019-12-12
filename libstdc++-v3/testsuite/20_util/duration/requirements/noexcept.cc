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

// { dg-do compile { target c++11 } }

// P0972R0 <chrono> zero(), min(), and max() should be noexcept

#include <chrono>

using namespace std;

using vals = chrono::duration_values<short>;
static_assert( noexcept(vals::zero()), "duration_values::zero()" );
static_assert( noexcept(vals::min()), "duration_values::min()" );
static_assert( noexcept(vals::max()), "duration_values::max()" );

using dur1 = chrono::system_clock::duration;
static_assert( noexcept(dur1::zero()), "duration::zero()" );
static_assert( noexcept(dur1::min()), "duration::min()" );
static_assert( noexcept(dur1::max()), "duration::max()" );

using dur2 = chrono::duration<short, ratio<10>>;
static_assert( noexcept(dur2::zero()), "duration::zero()" );
static_assert( noexcept(dur2::min()), "duration::min()" );
static_assert( noexcept(dur2::max()), "duration::max()" );

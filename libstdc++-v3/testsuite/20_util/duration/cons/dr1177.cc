// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

#include <chrono>
#include <type_traits>

using namespace std;
using namespace std::chrono;

// DR 1177
static_assert(is_constructible<duration<float>, duration<double>>{},
    "can convert duration with one floating point rep to another");
static_assert(is_constructible<duration<float>, duration<int>>{},
    "can convert duration with integral rep to one with floating point rep");
static_assert(!is_constructible<duration<int>, duration<float>>{},
    "cannot convert duration with floating point rep to one with integral rep");
static_assert(is_constructible<duration<int>, duration<long>>{},
    "can convert duration with one integral rep to another");

static_assert(!is_constructible<duration<int>, duration<int, ratio<2,3>>>{},
    "cannot convert duration to one with different period");
static_assert(is_constructible<duration<float>, duration<int, ratio<2,3>>>{},
    "... unless the result type has a floating-point representation");
static_assert(is_constructible<duration<int, ratio<1,3>>, duration<int>>{},
    "... or the original's period is a multiple of the result's period");

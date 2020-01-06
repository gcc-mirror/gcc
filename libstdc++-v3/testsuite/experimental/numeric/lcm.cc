// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }

#include <experimental/numeric>

using std::experimental::fundamentals_v2::lcm;

static_assert(lcm(21, 6) == 42, "");
static_assert(lcm(41, 0) == 0, "LCD with zero is zero");
static_assert(lcm(0, 7) == 0, "LCD with zero is zero");
static_assert(lcm(0, 0) == 0, "no division by zero");

static_assert(lcm(1u, 2) == 2, "unsigned and signed");
static_assert(lcm(3, 4u) == 12, "signed and unsigned");
static_assert(lcm(5u, 6u) == 30, "unsigned and unsigned");

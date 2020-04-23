// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }
//
// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <array>

#ifndef __cpp_lib_array_constexpr
# error "Feature test macro for array constexpr is missing in <array>"
#elif __cpp_lib_array_constexpr < 201806L
# error "Feature test macro for array constexpr has wrong value in <array>"
#endif

constexpr std::array<int, 3> a1{{1, 2, 3}};
constexpr std::array<int, 3> a2{{4, 5, 6}};
constexpr std::array<int, 3> a3{{1, 2, 4}};
constexpr std::array<int, 3> a4{{1, 3, 3}};

static_assert(a1 == a1);
static_assert(a1 != a2);
static_assert(a1 < a3);
static_assert(a4 > a1);
static_assert(a1 <= a3);
static_assert(a4 >= a1);
static_assert(std::is_eq(a1 <=> a1));
static_assert(std::is_neq(a1 <=> a2));
static_assert(std::is_lt(a1 <=> a3));
static_assert(std::is_gt(a4 <=> a1));

constexpr std::array<unsigned char, 3> a5{{1, 2, 3}};
constexpr std::array<unsigned char, 3> a6{{4, 5, 6}};
constexpr std::array<unsigned char, 3> a7{{1, 2, 4}};
constexpr std::array<unsigned char, 3> a8{{1, 3, 3}};

static_assert(a5 == a5);
static_assert(a5 != a6);
static_assert(a5 < a7);
static_assert(a8 > a5);
static_assert(a5 <= a7);
static_assert(a8 >= a5);
static_assert(std::is_eq(a5 <=> a5));
static_assert(std::is_neq(a5 <=> a6));
static_assert(std::is_lt(a5 <=> a7));
static_assert(std::is_gt(a8 <=> a5));

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

// { dg-do compile { target c++17 } }

// Test noexcept-specifier on std::apply

#include <tuple>

using std::tuple;
using std::declval;

void f1();

static_assert( !noexcept(apply(f1, declval<tuple<>>())) );
static_assert( !noexcept(apply(f1, declval<tuple<>&>())) );
static_assert( !noexcept(apply(f1, declval<const tuple<>>())) );
static_assert( !noexcept(apply(f1, declval<const tuple<>&>())) );

void f2() noexcept;

static_assert( noexcept(apply(f2, declval<tuple<>>())) );
static_assert( noexcept(apply(f2, declval<tuple<>&>())) );
static_assert( noexcept(apply(f2, declval<const tuple<>>())) );
static_assert( noexcept(apply(f2, declval<const tuple<>&>())) );

struct F3 {
  void operator()(int&);
  void operator()(int&&) noexcept;
  void operator()(const int&) noexcept;
  void operator()(const int&&);
} f3;

static_assert( noexcept(apply(f3, declval<tuple<int>>())) );
static_assert( !noexcept(apply(f3, declval<tuple<int>&>())) );
static_assert( !noexcept(apply(f3, declval<const tuple<int>>())) );
static_assert( noexcept(apply(f3, declval<const tuple<int>&>())) );

struct F4 {
  void operator()(int&, const int&);
  void operator()(int&&, int&&) noexcept;
} f4;

static_assert( noexcept(apply(f4, declval<tuple<int, int>>())) );
static_assert( !noexcept(apply(f4, declval<tuple<int, int>&>())) );
static_assert( !noexcept(apply(f4, declval<tuple<int&, const int>>())) );
static_assert( !noexcept(apply(f4, declval<tuple<int, const int>&>())) );

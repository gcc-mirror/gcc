// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// Test noexcept-specifier on std::make_from_tuple

#include <tuple>

using std::make_from_tuple;
using std::tuple;
using std::declval;

struct T1 { T1(); };

static_assert( !noexcept(make_from_tuple<T1>(declval<tuple<>>())) );
static_assert( !noexcept(make_from_tuple<T1>(declval<tuple<>&>())) );
static_assert( !noexcept(make_from_tuple<T1>(declval<const tuple<>>())) );
static_assert( !noexcept(make_from_tuple<T1>(declval<const tuple<>&>())) );

struct T2 { };

static_assert( noexcept(make_from_tuple<T2>(declval<tuple<>>())) );
static_assert( noexcept(make_from_tuple<T2>(declval<tuple<>&>())) );
static_assert( noexcept(make_from_tuple<T2>(declval<const tuple<>>())) );
static_assert( noexcept(make_from_tuple<T2>(declval<const tuple<>&>())) );

struct T3 {
  T3(int&);
  T3(int&&) noexcept;
  T3(const int&) noexcept;
  T3(const int&&);
};

static_assert( noexcept(make_from_tuple<T3>(declval<tuple<int>>())) );
static_assert( !noexcept(make_from_tuple<T3>(declval<tuple<int>&>())) );
static_assert( !noexcept(make_from_tuple<T3>(declval<const tuple<int>>())) );
static_assert( noexcept(make_from_tuple<T3>(declval<const tuple<int>&>())) );

struct T4 {
  T4(int&, const int&);
  T4(int&&, int&&) noexcept;
};

static_assert( noexcept(make_from_tuple<T4>(declval<tuple<int, int>>())) );
static_assert( !noexcept(make_from_tuple<T4>(declval<tuple<int, int>&>())) );
static_assert( !noexcept(make_from_tuple<T4>(declval<tuple<int&, const int>>())) );
static_assert( !noexcept(make_from_tuple<T4>(declval<tuple<int, const int>&>())) );

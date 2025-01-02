// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <concepts>

static_assert( ! std::regular_invocable<int> );
static_assert( std::regular_invocable<int()> );
static_assert( std::regular_invocable<int(*)()> );
static_assert( std::regular_invocable<int(&)()> );
static_assert( ! std::regular_invocable<int(), int> );
static_assert( ! std::regular_invocable<int() const> );

static_assert( std::regular_invocable<int(const long*, short), long*, char> );

struct A;
static_assert( std::regular_invocable<int A::*, const A&> );
static_assert( ! std::regular_invocable<void (A::*)(long&), const A*, long&> );
static_assert( std::regular_invocable<void (A::*)(long&) const, A*, long&> );

struct F
{
  void operator()() const;
  void operator()(int);
};
static_assert( std::regular_invocable<F> );
static_assert( std::regular_invocable<F, int> );
static_assert( std::regular_invocable<const F&> );
static_assert( ! std::regular_invocable<const F&, int> );

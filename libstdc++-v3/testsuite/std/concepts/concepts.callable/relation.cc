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

// { dg-do compile { target c++20 } }

#include <concepts>

static_assert( ! std::relation<bool, void, void> );
static_assert( ! std::relation<bool(), void, void> );
static_assert( ! std::relation<bool(*)(), void, void> );
static_assert( ! std::relation<bool(&)(int, int), void, void> );
static_assert( ! std::relation<bool(), int, int> );
static_assert( ! std::relation<bool() const, int, int> );

static_assert( std::relation<bool(*)(int, int), short, long> );
static_assert( std::relation<bool(&)(const void*, const void*), char[2], int*> );

static_assert( ! std::relation<bool& (const long*, short), long*, char> );

struct A;
static_assert( ! std::relation<int A::*, const A&, const A&> );
static_assert( ! std::relation<void (A::*)(long&), const A*, long&> );
static_assert( ! std::relation<void (A::*)(long&) const, A*, long&> );
static_assert( std::relation<long (A::*)(A*) const, A*, A*> );

struct F
{
  void operator()(long, long) const;
  bool& operator()(int, const int&) const;
};
static_assert( ! std::relation<F, long, long> );
static_assert( std::relation<F&, int, int> );
static_assert( std::relation<const F&, const int, const int> );

// [concept.equiv]
static_assert( std::equivalence_relation<bool(*)(int, int), short, long> );
static_assert( ! std::equivalence_relation<F, long, long> );
static_assert( std::equivalence_relation<const F&, const int, const int> );

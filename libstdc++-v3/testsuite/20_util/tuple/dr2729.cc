// Copyright (C) 2018 Free Software Foundation, Inc.
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

#include <tuple>
#include <testsuite_tr1.h>

using std::tuple;
using std::pair;
using __gnu_test::assign::AnyAssign;
using __gnu_test::assign::DelAnyAssign;
using __gnu_test::assign::DelCopyAssign;
using __gnu_test::CopyConsOnlyType;

// Copy assignment:
template<typename T>
constexpr bool copy() { return std::is_copy_assignable<T>::value; }

// Move assigmment:
template<typename T>
constexpr bool move() { return std::is_move_assignable<T>::value; }

static_assert( copy<tuple<>>(), "");
static_assert( move<tuple<>>(), "");

static_assert( copy<tuple<int>>(), "");
static_assert( copy<tuple<AnyAssign>>(), "");
static_assert( copy<tuple<int, int>>(), "");
static_assert( copy<tuple<AnyAssign, AnyAssign>>(), "");
static_assert( copy<tuple<int, AnyAssign>>(), "");
static_assert( copy<tuple<AnyAssign, int>>(), "");
static_assert( copy<tuple<int, int, int>>(), "");
static_assert( copy<tuple<AnyAssign, AnyAssign, AnyAssign>>(), "");
static_assert( copy<tuple<int, AnyAssign, AnyAssign>>(), "");
static_assert( copy<tuple<AnyAssign, int, AnyAssign>>(), "");
static_assert( copy<tuple<AnyAssign, AnyAssign, int>>(), "");

static_assert( move<tuple<int>>(), "");
static_assert( move<tuple<AnyAssign>>(), "");
static_assert( move<tuple<int, int>>(), "");
static_assert( move<tuple<AnyAssign, AnyAssign>>(), "");
static_assert( move<tuple<int, AnyAssign>>(), "");
static_assert( move<tuple<AnyAssign, int>>(), "");
static_assert( move<tuple<int, int, int>>(), "");
static_assert( move<tuple<AnyAssign, AnyAssign, AnyAssign>>(), "");
static_assert( move<tuple<int, AnyAssign, AnyAssign>>(), "");
static_assert( move<tuple<AnyAssign, int, AnyAssign>>(), "");
static_assert( move<tuple<AnyAssign, AnyAssign, int>>(), "");

static_assert( ! copy<tuple<DelCopyAssign>>(), "");
static_assert( ! copy<tuple<DelCopyAssign, int>>(), "");
static_assert( ! copy<tuple<int, DelCopyAssign>>(), "");
static_assert( ! copy<tuple<DelCopyAssign, int, int>>(), "");
static_assert( ! copy<tuple<int, DelCopyAssign, int>>(), "");
static_assert( ! copy<tuple<int, int, DelCopyAssign>>(), "");

static_assert( move<tuple<DelCopyAssign>>(), "");
static_assert( move<tuple<DelCopyAssign, int>>(), "");
static_assert( move<tuple<int, DelCopyAssign>>(), "");
static_assert( move<tuple<DelCopyAssign, int, int>>(), "");
static_assert( move<tuple<int, DelCopyAssign, int>>(), "");
static_assert( move<tuple<int, int, DelCopyAssign>>(), "");

static_assert( ! move<tuple<CopyConsOnlyType>>(), "");
static_assert( ! move<tuple<CopyConsOnlyType, int>>(), "");
static_assert( ! move<tuple<int, CopyConsOnlyType>>(), "");
static_assert( ! move<tuple<CopyConsOnlyType, int, int>>(), "");
static_assert( ! move<tuple<int, CopyConsOnlyType, int>>(), "");
static_assert( ! move<tuple<int, int, CopyConsOnlyType>>(), "");

// Assignment from different types of tuple (and pair):
template<typename To, typename From>
constexpr bool assign() { return std::is_assignable<To&, From>::value; }

// 0-tuples
static_assert( ! assign<tuple<>, tuple<int>>(), "" );
static_assert( ! assign<tuple<>, const tuple<int>&>(), "" );

// 1-tuples
static_assert( ! assign<tuple<int>, tuple<>>(), "" );
static_assert( ! assign<tuple<int>, const tuple<>&>(), "" );
static_assert( ! assign<tuple<AnyAssign>, tuple<>>(), "" );
static_assert( ! assign<tuple<AnyAssign>, tuple<int, int>>(), "" );
static_assert( ! assign<tuple<AnyAssign>, pair<int, int>>(), "" );

static_assert( ! assign<tuple<void*>, tuple<int>>(), "" );
static_assert( ! assign<tuple<void*>, const tuple<int>&>(), "" );

static_assert( assign<tuple<long>, tuple<int>>(), "" );
static_assert( assign<tuple<long>, tuple<int>&>(), "" );
static_assert( assign<tuple<long>, const tuple<int>>(), "" );
static_assert( assign<tuple<long>, const tuple<int>&>(), "" );

// 2-tuples
static_assert( assign<tuple<long, long>, tuple<int, int>>(), "" );
static_assert( assign<tuple<long, long>, tuple<int, int>&>(), "" );
static_assert( assign<tuple<long, long>, const tuple<int, int>>(), "" );
static_assert( assign<tuple<long, long>, const tuple<int, int>&>(), "" );

static_assert( assign<tuple<long, long>, pair<int, int>>(), "" );
static_assert( assign<tuple<long, long>, const pair<int, int>&>(), "" );
static_assert( assign<tuple<long, long>, pair<int, int>>(), "" );
static_assert( assign<tuple<long, long>, const pair<int, int>&&>(), "" );

static_assert( assign<tuple<DelCopyAssign, AnyAssign>,
		      tuple<DelCopyAssign, int>>(), "" );
static_assert( ! assign<tuple<DelCopyAssign, AnyAssign>,
			tuple<DelCopyAssign, int>&>(), "" );
static_assert( ! assign<tuple<DelCopyAssign, AnyAssign>,
			const tuple<DelCopyAssign, int>&>(), "" );
static_assert( ! assign<tuple<DelCopyAssign, AnyAssign>,
			const tuple<DelCopyAssign, int>&&>(), "" );

static_assert( assign<tuple<AnyAssign, DelCopyAssign>,
		      tuple<int, DelCopyAssign>>(), "" );
static_assert( ! assign<tuple<AnyAssign, DelCopyAssign>,
			tuple<int, DelCopyAssign>&>(), "" );
static_assert( ! assign<tuple<AnyAssign, DelCopyAssign>,
			const tuple<int, DelCopyAssign>&>(), "" );
static_assert( ! assign<tuple<AnyAssign, DelCopyAssign>,
			const tuple<int, DelCopyAssign>&&>(), "" );

static_assert( ! assign<tuple<void*, int>,
			tuple<int, int>>(), "" );
static_assert( ! assign<tuple<void*, int>,
			const tuple<int, int>&>(), "" );

static_assert( assign<tuple<DelCopyAssign, AnyAssign>,
		      pair<DelCopyAssign, int>>(), "" );
static_assert( ! assign<tuple<DelCopyAssign, AnyAssign>,
			pair<DelCopyAssign, int>&>(), "" );
static_assert( ! assign<tuple<DelCopyAssign, AnyAssign>,
			const pair<DelCopyAssign, int>&>(), "" );
static_assert( ! assign<tuple<DelCopyAssign, AnyAssign>,
			const pair<DelCopyAssign, int>&&>(), "" );

static_assert( assign<tuple<AnyAssign, DelCopyAssign>,
		      pair<int, DelCopyAssign>>(), "" );
static_assert( ! assign<tuple<AnyAssign, DelCopyAssign>,
			pair<int, DelCopyAssign>&>(), "" );
static_assert( ! assign<tuple<AnyAssign, DelCopyAssign>,
			const pair<int, DelCopyAssign>&>(), "" );
static_assert( ! assign<tuple<AnyAssign, DelCopyAssign>,
			const pair<int, DelCopyAssign>&&>(), "" );

static_assert( ! assign<tuple<void*, int>,
			pair<int, int>>(), "" );
static_assert( ! assign<tuple<void*, int>,
			const pair<int, int>&>(), "" );

// 3-tuples
static_assert( assign<tuple<AnyAssign, DelCopyAssign, AnyAssign>,
		      tuple<int, DelCopyAssign, int>>(), "" );
static_assert( ! assign<tuple<AnyAssign, DelCopyAssign, AnyAssign>,
			tuple<int, DelCopyAssign, int>&>(), "" );
static_assert( ! assign<tuple<AnyAssign, DelCopyAssign, AnyAssign>,
			const tuple<int, DelCopyAssign, int>&>(), "" );
static_assert( ! assign<tuple<AnyAssign, DelCopyAssign, AnyAssign>,
			const tuple<int, DelCopyAssign, int>&&>(), "" );

static_assert( ! assign<tuple<int, void*, int>,
			tuple<int, int, int>>(), "" );
static_assert( ! assign<tuple<int, void*, int>,
			const tuple<int, int, int>&>(), "" );

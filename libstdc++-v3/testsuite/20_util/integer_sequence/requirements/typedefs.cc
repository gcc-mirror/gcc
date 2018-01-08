// { dg-do compile { target c++14 } }
//
// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <utility>
#include <type_traits>

using std::is_same;
using std::integer_sequence;
using std::make_integer_sequence;
using std::index_sequence;
using std::make_index_sequence;
using std::index_sequence_for;

static_assert( is_same<integer_sequence<int>::value_type, int>::value,
	       "int value_type");

static_assert( is_same<integer_sequence<short>::value_type, short>::value,
	       "short value_type");

static_assert( is_same<make_integer_sequence<int, 0>,
		       integer_sequence<int>>::value,
	       "make empty int seq" );

static_assert( is_same<make_integer_sequence<int, 2>,
		       integer_sequence<int, 0, 1>>::value,
	       "make non-empty int seq" );

static_assert( is_same<make_integer_sequence<unsigned, 0>,
		       integer_sequence<unsigned>>::value,
	       "make empty unsigned seq" );

static_assert( is_same<make_integer_sequence<unsigned, 2>,
		       integer_sequence<unsigned, 0, 1>>::value,
	       "make non-empty unsigned seq" );

static_assert( is_same<index_sequence<0, 1>,
		       integer_sequence<std::size_t, 0, 1>>::value,
	       "index seq" );

static_assert( is_same<make_index_sequence<2>, index_sequence<0, 1>>::value,
	       "make index seq" );

static_assert( is_same<index_sequence_for<char, int, void, double>,
		       index_sequence<0, 1, 2, 3>>::value,
	       "index_sequence_for" );

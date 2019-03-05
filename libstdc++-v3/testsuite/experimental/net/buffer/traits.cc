// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

#include <experimental/buffer>

using namespace std::experimental::net;
using std::vector;
using std::string;
using std::char_traits;
using std::allocator;

template<typename T>
struct Seq {
  struct Buf {
    operator T() const { return {}; }
  };

  Buf* begin() const { return nullptr; }
  Buf* end() const { return nullptr; }
};

static_assert( is_mutable_buffer_sequence<mutable_buffer>::value,
    "mutable_buffer is a mutable buffer sequence" );
static_assert( is_mutable_buffer_sequence<const mutable_buffer>::value,
    "const mutable_buffer is a mutable buffer sequence" );
static_assert( is_mutable_buffer_sequence<vector<mutable_buffer>>::value,
    "vector<mutable_buffer> is a mutable buffer sequence" );
static_assert( is_mutable_buffer_sequence<const vector<mutable_buffer>>::value,
    "const vector<mutable_buffer> is a mutable buffer sequence" );
static_assert( is_mutable_buffer_sequence<Seq<mutable_buffer>>::value,
    "Seq<mutable_buffer> is a mutable buffer sequence" );
static_assert( is_mutable_buffer_sequence<const Seq<mutable_buffer>>::value,
    "const Seq<mutable_buffer> is a mutable buffer sequence" );
static_assert( is_mutable_buffer_sequence<Seq<const mutable_buffer>>::value,
    "Seq<const mutable_buffer> is a mutable buffer sequence" );
static_assert( ! is_mutable_buffer_sequence<const_buffer>::value,
    "const_buffer is not a mutable buffer sequence" );
static_assert( ! is_mutable_buffer_sequence<vector<const_buffer>>::value,
    "vector<const_buffer> is not a mutable buffer sequence" );
static_assert( ! is_mutable_buffer_sequence<Seq<const_buffer>>::value,
    "Seq<const_buffer> is not a mutable buffer sequence" );

static_assert( is_const_buffer_sequence<const_buffer>::value,
    "const_buffer is a const buffer sequence" );
static_assert( is_const_buffer_sequence<const const_buffer>::value,
    "const const_buffer is a const buffer sequence" );
static_assert( is_const_buffer_sequence<vector<const_buffer>>::value,
    "vector<const_buffer> is a const buffer sequence" );
static_assert( is_const_buffer_sequence<const vector<const_buffer>>::value,
    "const vector<const_buffer> is a const buffer sequence" );
static_assert( is_const_buffer_sequence<Seq<const_buffer>>::value,
    "Seq<const_buffer> is a const buffer sequence" );
static_assert( is_const_buffer_sequence<const Seq<const_buffer>>::value,
    "const Seq<const_buffer> is a const buffer sequence" );
static_assert( is_const_buffer_sequence<Seq<const const_buffer>>::value,
    "Seq<const const_buffer> is a const buffer sequence" );
static_assert( is_const_buffer_sequence<mutable_buffer>::value,
    "mutable_buffer is a const buffer sequence" );
static_assert( is_const_buffer_sequence<const mutable_buffer>::value,
    "const mutable_buffer is a const buffer sequence" );
static_assert( is_const_buffer_sequence<vector<mutable_buffer>>::value,
    "vector<mutable_buffer> is a const buffer sequence" );
static_assert( is_const_buffer_sequence<const vector<mutable_buffer>>::value,
    "const vector<mutable_buffer> is a const buffer sequence" );

// Buf -> mutable_buffer -> const_buffer needs two user-defined conversions:
static_assert( ! is_const_buffer_sequence<Seq<mutable_buffer>>::value,
    "Seq<mutable_buffer> is not a const buffer sequence" );

static_assert( is_dynamic_buffer<
    dynamic_vector_buffer<int, allocator<int>>
    >::value, "dynamic_vector_buffer is a dynamic buffer" );
static_assert( is_dynamic_buffer<
    dynamic_string_buffer<char, char_traits<char>, allocator<int>>
    >::value, "dynamic_string_buffer is a dynamic buffer" );
static_assert( ! is_dynamic_buffer<vector<int>>::value,
    "vector is not a dynamic buffer" );
static_assert( ! is_dynamic_buffer<string>::value,
    "string is not a dynamic buffer" );

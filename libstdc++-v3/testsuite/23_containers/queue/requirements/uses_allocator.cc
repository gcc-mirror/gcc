// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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

#include <queue>

using test_type = std::queue<int>;
using container = test_type::container_type;

template<typename A>
  using uses_allocator = std::uses_allocator<test_type, A>;

template<typename... Args>
  using is_constructible = std::is_constructible<test_type, Args...>;

// test with valid allocator
using alloc_type = container::allocator_type;

static_assert( uses_allocator<alloc_type>::value, "valid allocator" );

static_assert( is_constructible<const alloc_type&>::value,
               "queue(const Alloc&)" );
static_assert( is_constructible<const container&, const alloc_type&>::value,
               "queue(const container_type&, const Alloc&)" );
static_assert( is_constructible<container&&, const alloc_type&>::value,
               "queue(const container_type&, const Alloc&)" );
static_assert( is_constructible<const test_type&, const alloc_type&>::value,
               "queue(const queue&, const Alloc&)" );
static_assert( is_constructible<test_type&&, const alloc_type&>::value,
               "queue(const queue&, const Alloc&)" );

// test with invalid allocator
struct X { };

static_assert( !uses_allocator<X>::value, "invalid allocator" );

static_assert( !is_constructible<const X&>::value,
               "queue(const NonAlloc&)" );
static_assert( !is_constructible<const container&, const X&>::value,
               "queue(const container_type&, const NonAlloc&)" );
static_assert( !is_constructible<container&&, const X&>::value,
               "queue(const container_type&, const NonAlloc&)" );
static_assert( !is_constructible<const test_type&, const X&>::value,
               "queue(const queue&, const NonAlloc&)" );
static_assert( !is_constructible<test_type&&, const X&>::value,
               "queue(const queue&, const NonAlloc&)" );

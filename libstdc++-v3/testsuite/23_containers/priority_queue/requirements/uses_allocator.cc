// Copyright (C) 2014-2021 Free Software Foundation, Inc.
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

using test_type = std::priority_queue<int>;
using container = test_type::container_type;
using comp = std::less<container::value_type>;

template<typename A>
  using uses_allocator = std::uses_allocator<test_type, A>;

template<typename... Args>
  using is_constructible = std::is_constructible<test_type, Args...>;

// test with invalid allocator
using alloc_type = container::allocator_type;

static_assert( uses_allocator<alloc_type>::value, "valid allocator" );

static_assert( is_constructible<const alloc_type&>::value,
               "priority_queue(const Alloc&)" );
static_assert( is_constructible<const comp&, const alloc_type&>::value,
               "priority_queue(const Cmp&, const Alloc&)" );
static_assert( is_constructible<const comp&, const container&,
                                const alloc_type&>::value,
               "priority_queue(const Cmp&, const Container&, const Alloc&)" );
static_assert( is_constructible<const comp&, container&&,
                                const alloc_type&>::value,
               "priority_queue(const Cmp&, const Container&, const Alloc&)" );
static_assert( is_constructible<const test_type&, const alloc_type&>::value,
               "priority_queue(const priority_queue&, const Alloc&)" );
static_assert( is_constructible<test_type&&, const alloc_type&>::value,
               "priority_queue(const priority_queue&, const Alloc&)" );

// test with invalid allocator
struct X { };

static_assert( !uses_allocator<X>::value, "invalid allocator" );

static_assert( !is_constructible<const X&>::value,
               "priority_queue(const NonAlloc&)" );
static_assert( !is_constructible<const comp&, const X&>::value,
               "priority_queue(const Cmp&, const NonAlloc&)" );
static_assert( !is_constructible<const comp&, const container&,
                                 const X&>::value,
               "priority_queue(const Cmp&, const Cont&, const NonAlloc&)" );
static_assert( !is_constructible<const comp&, container&&, const X&>::value,
               "priority_queue(const Cmp&, const Cont&, const NonAlloc&)" );
static_assert( !is_constructible<const test_type&, const X&>::value,
               "priority_queue(const priority_queue&, const NonAlloc&)" );
static_assert( !is_constructible<test_type&&, const X&>::value,
               "priority_queue(const priority_queue&, const NonAlloc&)" );

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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <memory_resource>

using std::pmr::monotonic_buffer_resource;
using std::pmr::memory_resource;
using std::size_t;

static_assert(std::is_base_of_v<memory_resource, monotonic_buffer_resource>);
static_assert(!std::is_abstract_v<monotonic_buffer_resource>);

static_assert(std::is_default_constructible_v<monotonic_buffer_resource>);
static_assert(std::is_destructible_v<monotonic_buffer_resource>);
static_assert(!std::is_copy_constructible_v<monotonic_buffer_resource>);
static_assert(!std::is_copy_assignable_v<monotonic_buffer_resource>);
static_assert(!std::is_move_constructible_v<monotonic_buffer_resource>);
static_assert(!std::is_move_assignable_v<monotonic_buffer_resource>);

static_assert(std::is_constructible_v<monotonic_buffer_resource,
				      memory_resource*>);
static_assert(std::is_constructible_v<monotonic_buffer_resource,
				      size_t, memory_resource*>);
static_assert(std::is_constructible_v<monotonic_buffer_resource,
				      void*, size_t, memory_resource*>);

static_assert(std::is_constructible_v<monotonic_buffer_resource,
				      size_t>);
static_assert(std::is_constructible_v<monotonic_buffer_resource,
				      void*, size_t>);

// Unary constructors are explicit.
static_assert(!std::is_convertible_v<memory_resource*,
				     monotonic_buffer_resource>);
static_assert(!std::is_convertible_v<size_t,
				     monotonic_buffer_resource>);

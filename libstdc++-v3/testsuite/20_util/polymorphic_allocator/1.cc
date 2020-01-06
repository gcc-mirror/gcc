// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

struct X { int i = 0; };

using test_type = std::pmr::polymorphic_allocator<X>;

static_assert(std::is_default_constructible_v<test_type>);
static_assert(std::is_destructible_v<test_type>);
static_assert(std::is_copy_constructible_v<test_type>);
static_assert(!std::is_copy_assignable_v<test_type>);
static_assert(std::is_constructible_v<test_type, std::pmr::memory_resource*>);

static_assert(std::is_same_v<test_type::value_type, X>);

static_assert(!std::is_polymorphic_v<test_type>);
static_assert(!std::is_final_v<test_type>);

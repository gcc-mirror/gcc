// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>

#include <vector>
#include "../../../include/std/pmroptional"

struct non_td
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

 ~non_td(){};

};
struct td
{
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

};
static_assert(std::uses_allocator<non_td, std::pmr::polymorphic_allocator<void>>{}, "");
static_assert(std::uses_allocator<td, std::pmr::polymorphic_allocator<void>>{}, "");
static_assert(std::is_trivially_destructible_v<non_td> == false, "");
static_assert(std::is_trivially_destructible_v<td> == true, "");
static_assert(std::is_trivially_destructible_v<std::pmr::optional<non_td>> == false, "");
static_assert(std::is_trivially_destructible_v<std::pmr::optional<td>> == true, "");

// { dg-do compile { target c++17 } }

// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

#include <memory_resource>

static_assert(std::is_abstract_v<std::pmr::memory_resource>);
static_assert(std::is_polymorphic_v<std::pmr::memory_resource>);
static_assert(!std::is_final_v<std::pmr::memory_resource>);

struct R0 : std::pmr::memory_resource { };
static_assert(std::is_abstract_v<R0>);

struct R1 : R0 {
  void* do_allocate(std::size_t, std::size_t) override;
};
static_assert(std::is_abstract_v<R1>);

struct R2 : R1 {
  void do_deallocate(void*, std::size_t, std::size_t) override;
};
static_assert(std::is_abstract_v<R2>);

struct R3 : R2 {
  bool do_is_equal(const std::pmr::memory_resource&) const noexcept override;
};
static_assert(!std::is_abstract_v<R3>);
static_assert(std::is_default_constructible_v<R3>);
static_assert(std::is_copy_constructible_v<R3>);
static_assert(std::is_copy_assignable_v<R3>);
static_assert(std::is_destructible_v<R3>);

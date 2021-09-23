// { dg-do compile { target c++17 }  }

// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

#include <optional>

struct X
{
  ~X();
};

struct Y
{
  Y(const Y&) = default;
  Y(Y&&);
};

struct Z
{
  Z(const Z&);
  Z(Z&&) = default;
};

static_assert(std::is_trivially_copy_constructible_v<std::optional<int>>);
static_assert(std::is_trivially_move_constructible_v<std::optional<int>>);
static_assert(!std::is_trivially_copy_constructible_v<std::optional<X>>);
static_assert(!std::is_trivially_move_constructible_v<std::optional<X>>);
static_assert(std::is_trivially_copy_constructible_v<std::optional<Y>>);
static_assert(!std::is_trivially_move_constructible_v<std::optional<Y>>);
static_assert(!std::is_trivially_copy_constructible_v<std::optional<Z>>);
static_assert(std::is_trivially_move_constructible_v<std::optional<Z>>);

// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <ranges>

static_assert(std::ranges::view<std::ranges::empty_view<int>>);

std::ranges::empty_view<int> e;
static_assert(std::ranges::empty(e));
static_assert(0 == e.size());

static_assert(e.begin() == nullptr);
static_assert(e.end() == nullptr);
static_assert(e.data() == nullptr);
static_assert(e.empty());

static_assert(std::ranges::begin(e) == nullptr);
static_assert(std::ranges::end(e) == nullptr);

// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <compare>

template<typename Cat, typename... T>
constexpr bool check()
{
  return std::same_as<Cat, std::common_comparison_category_t<T...>>;
}

using std::partial_ordering;
using std::weak_ordering;
using std::strong_ordering;

static_assert(check<strong_ordering>());
static_assert(check<void, int>());
static_assert(check<void, int, int>());
static_assert(check<void, weak_ordering, int>());
static_assert(check<void, int, partial_ordering>());
static_assert(check<partial_ordering, partial_ordering>());
static_assert(check<partial_ordering, weak_ordering, partial_ordering>());

using PO = std::partial_ordering;
using WO = std::weak_ordering;
using SO = std::strong_ordering;

static_assert(check<PO, SO, PO, SO, SO>());
static_assert(check<PO, SO, PO, SO, WO>());
static_assert(check<WO, SO, WO, SO, WO>());
static_assert(check<SO, SO, SO, SO, SO>());

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <compare>

static_assert(noexcept(std::partial_ordering::less == 0));
static_assert(noexcept(std::partial_ordering::less != 0));
static_assert(noexcept(std::partial_ordering::less < 0));
static_assert(noexcept(std::partial_ordering::less > 0));
static_assert(noexcept(std::partial_ordering::less <= 0));
static_assert(noexcept(std::partial_ordering::less >= 0));
static_assert(noexcept(std::partial_ordering::less <=> 0));
static_assert(noexcept(0 == std::partial_ordering::less));
static_assert(noexcept(0 != std::partial_ordering::less));
static_assert(noexcept(0 < std::partial_ordering::less));
static_assert(noexcept(0 > std::partial_ordering::less));
static_assert(noexcept(0 <= std::partial_ordering::less));
static_assert(noexcept(0 >= std::partial_ordering::less));
static_assert(noexcept(0 <=> std::partial_ordering::less));

static_assert(noexcept(std::weak_ordering::less == 0));
static_assert(noexcept(std::weak_ordering::less != 0));
static_assert(noexcept(std::weak_ordering::less < 0));
static_assert(noexcept(std::weak_ordering::less > 0));
static_assert(noexcept(std::weak_ordering::less <= 0));
static_assert(noexcept(std::weak_ordering::less >= 0));
static_assert(noexcept(std::weak_ordering::less <=> 0));
static_assert(noexcept(0 == std::weak_ordering::less));
static_assert(noexcept(0 != std::weak_ordering::less));
static_assert(noexcept(0 < std::weak_ordering::less));
static_assert(noexcept(0 > std::weak_ordering::less));
static_assert(noexcept(0 <= std::weak_ordering::less));
static_assert(noexcept(0 >= std::weak_ordering::less));
static_assert(noexcept(0 <=> std::weak_ordering::less));

static_assert(noexcept(std::strong_ordering::less == 0));
static_assert(noexcept(std::strong_ordering::less != 0));
static_assert(noexcept(std::strong_ordering::less < 0));
static_assert(noexcept(std::strong_ordering::less > 0));
static_assert(noexcept(std::strong_ordering::less <= 0));
static_assert(noexcept(std::strong_ordering::less >= 0));
static_assert(noexcept(std::strong_ordering::less <=> 0));
static_assert(noexcept(0 == std::strong_ordering::less));
static_assert(noexcept(0 != std::strong_ordering::less));
static_assert(noexcept(0 < std::strong_ordering::less));
static_assert(noexcept(0 > std::strong_ordering::less));
static_assert(noexcept(0 <= std::strong_ordering::less));
static_assert(noexcept(0 >= std::strong_ordering::less));
static_assert(noexcept(0 <=> std::strong_ordering::less));

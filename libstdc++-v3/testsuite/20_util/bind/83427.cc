// Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <functional>

// PR libstdc++/83427

int f() noexcept { return 0; }
auto b = std::bind(f);
static_assert(std::is_same_v<decltype(b)::result_type, int>);

struct X { long f() const & noexcept { return 0L; } };
auto b2 = std::bind(&X::f, X{});
static_assert(std::is_same_v<decltype(b2)::result_type, long>);

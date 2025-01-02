// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

#include <set>

std::pmr::multiset<int> c{1, 2, 3, 4};

struct X { };
struct Cmp { bool operator()(X, X) const { return false; } };

static_assert(std::is_same_v<std::pmr::multiset<X>,
    std::multiset<X, std::less<X>, std::pmr::polymorphic_allocator<X>>>);
static_assert(std::is_same_v<std::pmr::multiset<X, Cmp>,
    std::multiset<X, Cmp, std::pmr::polymorphic_allocator<X>>>);

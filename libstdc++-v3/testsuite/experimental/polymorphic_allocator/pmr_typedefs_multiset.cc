// Copyright (C) 2018-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++14 } }

#include <experimental/set>

namespace xpmr = std::experimental::pmr;

struct X { };
struct Cmp { bool operator()(X, X) const { return false; } };

static_assert(std::is_same<xpmr::multiset<X>,
    std::multiset<X, std::less<X>, xpmr::polymorphic_allocator<X>>>::value,
    "pmr::multiset");
static_assert(std::is_same<xpmr::multiset<X, Cmp>,
    std::multiset<X, Cmp, xpmr::polymorphic_allocator<X>>>::value,
    "pmr::multiset");

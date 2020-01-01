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

// { dg-do compile { target c++14 } }

#include <experimental/map>

namespace xpmr = std::experimental::pmr;

struct X { };
struct Y { };
struct Cmp { bool operator()(X, X) const { return false; } };

static_assert(std::is_same<xpmr::map<X, Y>,
    std::map<X, Y, std::less<X>,
	     xpmr::polymorphic_allocator<std::pair<const X, Y>>>>::value,
    "pmr::map");
static_assert(std::is_same<xpmr::map<X, Y, Cmp>,
    std::map<X, Y, Cmp,
	     xpmr::polymorphic_allocator<std::pair<const X, Y>>>>::value,
    "pmr::map");

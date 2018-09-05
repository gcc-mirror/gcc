// Copyright (C) 2018 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17 -D_GLIBCXX_DEBUG" }
// { dg-do compile { target c++17 } }

#include <debug/map>
static_assert(std::is_same_v<
    std::pmr::multimap<int, int>,
    __gnu_debug::multimap<int, int, std::less<int>,
      std::pmr::polymorphic_allocator<std::pair<const int, int>>>
    >);

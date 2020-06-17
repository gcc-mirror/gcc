// Copyright (C) 2020 Free Software Foundation, Inc.
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

// { dg-options "-std=c++2a" }
// { dg-do compile { target c++2a } }

#include <ranges>

void
test01()
{
  // PR libstdc++/93267
  std::ranges::iota_view<long long, int> i(0, 3);
  static_assert( std::weakly_incrementable<decltype(i.begin())> );
  (void) std::ranges::begin(i);
}

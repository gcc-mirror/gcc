// { dg-do compile { target c++17 } }

// Copyright (C) 2013-2024 Free Software Foundation, Inc.
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

// C++17 27.7, range access [iterator.range]

#include <string_view>

void
test01()
{
  std::string_view s("Hello, World!");
  (void) std::begin(s);
  (void) std::end(s);
  (void) std::rbegin(s);
  (void) std::rend(s);
}

void
test02()
{
  constexpr std::string_view s("Hello, World!");
  [[maybe_unused]] constexpr auto b = std::begin(s);
  [[maybe_unused]] constexpr auto e = std::end(s);
  [[maybe_unused]] constexpr auto cb = std::cbegin(s);
  [[maybe_unused]] constexpr auto ce = std::cend(s);
  [[maybe_unused]] constexpr auto rb = std::rbegin(s);
  [[maybe_unused]] constexpr auto re = std::rend(s);
  [[maybe_unused]] constexpr auto crb = std::crbegin(s);
  [[maybe_unused]] constexpr auto cre = std::crend(s);
}

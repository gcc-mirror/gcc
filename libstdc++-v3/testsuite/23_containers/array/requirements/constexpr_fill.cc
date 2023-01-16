// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }
//
// Copyright (C) 2019-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <array>

#ifndef __cpp_lib_array_constexpr
# error "Feature test macro for array constexpr is missing in <array>"
#elif __cpp_lib_array_constexpr < 201811L
# error "Feature test macro for array constexpr has wrong value in <array>"
#endif

constexpr bool
test_array()
{
  auto ok = true;

  std::array<float,3> fa{};
  fa.fill(3.333f);

  ok = ok && (fa[0] == fa[2]);

  return ok;
}

static_assert(test_array());

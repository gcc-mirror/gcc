// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }
//
// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

constexpr bool
test_array()
{
  auto ok = true;

  std::array<float,3> fa{{1.1f, 2.2f, 3.3f}};

  std::array<float,3> fb{{4.4f, 5.5f, 6.6f}};

  fb.swap(fa);

  ok = ok && (fa[0] == 4.4f);

  std::swap(fa, fb);

  ok = ok && (fa[0] == 1.1f);

  return ok;
}

static_assert(test_array());

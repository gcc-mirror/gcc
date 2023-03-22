// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#include <utility>

#ifndef __cpp_lib_constexpr_algorithms
# error "Feature test macro for constexpr std::exchange is missing in <utility>"
#elif __cpp_lib_constexpr_algorithms < 201806L
# error "Feature test macro for constexpr std::exchange has wrong value in <utility>"
#endif

constexpr bool
test()
{
  constexpr double e_v = 2.71828182846;
  double e = e_v;
  constexpr double pi_v = 3.14159265359;
  const auto x = std::exchange(e, pi_v);

  return x == e_v && e == pi_v;
}

static_assert(test());

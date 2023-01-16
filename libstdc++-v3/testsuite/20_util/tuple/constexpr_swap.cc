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

#include <tuple>

constexpr bool
test_tuple()
{
  auto ok = true;

  std::tuple<int, double, double> t1(1, 3.1415, 2.7182);
  std::tuple<int, double, double> t2(1, 4.1234, 3.3333);
  t1.swap(t2);
  std::swap(t1, t2);

  return ok;
}

static_assert(test_tuple());

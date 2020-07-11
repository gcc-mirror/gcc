// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }
//
// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#ifndef __cpp_lib_constexpr_tuple
# error "Feature test macro for constexpr allocator constructors is missing in <tuple>"
#elif __cpp_lib_constexpr_tuple < 201811L
# error "Feature test macro for constexpr allocator constructors has wrong value in <tuple>"
#endif

#include <memory>

const std::allocator<int> alloc{};

constexpr bool
test_tuple()
{
  auto ok = true;

  std::tuple<int, double, double> ta(std::allocator_arg, alloc);
  std::tuple<int, double, double> tb(std::allocator_arg, alloc, 0, 3.456, 6.789);
  std::tuple<int, double, double> tc(std::allocator_arg, alloc, 0, 3.456f, 6.789f);
  std::tuple<int, double, double> td(std::allocator_arg, alloc, tb);
  std::tuple<int, double, double> te(std::allocator_arg, alloc, std::move(tb));

  std::tuple<int, float, float> tf(std::allocator_arg, alloc, 0, 3.456f, 6.789f);
  std::tuple<int, double, double> tg(std::allocator_arg, alloc, tf);
  std::tuple<int, double, double> th(std::allocator_arg, alloc, std::move(tf));

  std::pair<int, float> pf(12, 3.142f);
  std::tuple<int, double> ti(std::allocator_arg, alloc, pf);
  std::tuple<int, double> tj(std::allocator_arg, alloc, std::move(pf));

  return ok;
}

static_assert(test_tuple());

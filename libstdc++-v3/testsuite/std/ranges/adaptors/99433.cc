// Copyright (C) 2021 Free Software Foundation, Inc.
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

// PR libstdc++/99433

#include <ranges>
#include <vector>

template <typename underlying_adaptor_t>
struct deep
{
  underlying_adaptor_t adaptor;

  template <typename range_t>
  friend auto operator|(range_t &range, deep const &me)
  {
   return me.adaptor(range[0]);
  }
};

auto f = [] (auto nucl) { return nucl + ' '; };
auto complement = deep{std::views::transform(f)};
std::vector<std::vector<char>> foo{};
auto v = foo | complement;

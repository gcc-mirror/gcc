// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }
// FIXME [!HOSTED]: avoidable std::allocator usage
// { dg-require-effective-target hosted }

#include <memory>

constexpr bool
test01()
{
  const int sz{1};
  int* data{std::allocator<int>{}.allocate(sz)};
  static_assert(noexcept(std::ranges::construct_at(data, 42)));
  std::ranges::construct_at(data, 42);
  if (*data != 42)
    return false;
  std::ranges::destroy_at(data);
  std::allocator<int>{}.deallocate(data, sz);
  return true;
}

static_assert(test01());

struct S { S(); };
S* p;
static_assert(!noexcept(std::ranges::construct_at(p)));

// Copyright (C) 2017-2018 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile }

#include <type_traits>
#include "../../../../../include/std/pmroptional"
#if 0 //todo : enable when CTAD is supported
struct MoveOnly
{
  MoveOnly() = default;
  MoveOnly(MoveOnly&&);
  MoveOnly& operator=(MoveOnly&&);
};

int main()
{
  std::pmr::optional x = 5;
  static_assert(std::is_same_v<decltype(x), std::pmr::optional<int>>);
  int y = 42;
  std::pmr::optional x2 = y;
  static_assert(std::is_same_v<decltype(x2), std::pmr::optional<int>>);
  const int z = 666;
  std::pmr::optional x3 = z;
  static_assert(std::is_same_v<decltype(x3), std::pmr::optional<int>>);
  std::pmr::optional mo = MoveOnly();
  static_assert(std::is_same_v<decltype(mo), std::pmr::optional<MoveOnly>>);
  mo = MoveOnly();

  std::pmr::optional copy = x;
  static_assert(std::is_same_v<decltype(copy), std::pmr::optional<int>>);
  std::pmr::optional move = std::move(mo);
  static_assert(std::is_same_v<decltype(move), std::pmr::optional<MoveOnly>>);
}
#endif

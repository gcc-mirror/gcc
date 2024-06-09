// { dg-do compile { target c++20 } }
//
// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#include <string_view>
#include <array>

constexpr char
test()
{
  constexpr std::string_view hw("Hello, World!");
  static_assert('H' == *hw.begin());
  auto ch = hw[4];
  static_assert('W' == *(hw.cbegin() + 7));

  std::array<int, hw.size()> a2{{0,0,0,0,0,0,0,0,0,0,0,0,0}};
  auto hwi = hw.begin();
  auto hwe = hw.end();
  auto a2i = a2.begin();
  while (hwi != hwe)
    *a2i++ = *hwi++;

  return *(hw.cbegin() + 3);
}

void
run_test()
{
  constexpr char ch = test();
}

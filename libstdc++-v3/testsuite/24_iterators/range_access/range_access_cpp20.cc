// { dg-do compile { target c++20 } }

// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// N4830 23.7, Range access [iterator.range]

#include <iterator>

void
test01()
{
  static int i[1];
  constexpr auto s = std::ssize(i);
  const std::ptrdiff_t* check_type = &s;
  static_assert(s == 1);
}

void
test02()
{
  static int i[] = { 1, 2 };
  constexpr auto s = std::ssize(i);
  const std::ptrdiff_t* check_type = &s;
  static_assert(s == 2);
}

void
test03()
{
  struct Cont
  {
    constexpr unsigned short size() const { return 3; }
  };
  constexpr Cont c;
  constexpr auto s = std::ssize(c);
  const std::ptrdiff_t* check_type = &s;
  static_assert(s == 3);
}

void
test04()
{
  struct Cont
  {
    constexpr unsigned long long size() const { return 4; }
  };
  constexpr Cont c;
  constexpr auto s = std::ssize(c);
  const long long* check_type = &s;
  static_assert(s == 4);
}

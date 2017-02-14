// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

#include <array>

int main()
{
  // array
  typedef std::array<std::size_t, 6> array_type;
  constexpr array_type a = { { 0, 55, 66, 99, 4115, 2 } };
  constexpr auto v1 __attribute__((unused)) = a[1];
  constexpr auto v2 __attribute__((unused)) = a.at(2);
  constexpr auto v3 __attribute__((unused)) = a.front();
  constexpr auto v4 __attribute__((unused)) = a.back();
  return 0;
}

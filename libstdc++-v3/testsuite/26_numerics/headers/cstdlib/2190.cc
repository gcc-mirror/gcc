// 2000-01-01 bkoz

// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

// Math-related cstdlib bits are not freestanding.
// { dg-require-effective-target hosted }

// 17.4.1.2 Headers, cstdlib

#include <cstdlib>

// libstdc++/2190
void test01()
{
  long a __attribute__((unused)) = std::abs(1L);
  std::div(2L, 1L);
  std::ldiv_t b __attribute__((unused));
}

int main()
{
  test01();
  return 0;
}

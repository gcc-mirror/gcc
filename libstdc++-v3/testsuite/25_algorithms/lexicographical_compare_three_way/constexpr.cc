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

#include <algorithm>

constexpr bool
test01(int i)
{
  int a1[] = { 0, 1, 2, 3, 4, 5, 6, i, 8 };
  long a2[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };

  return std::lexicographical_compare_three_way(a1, a1+0, a2, a2+0) == 0
    && std::lexicographical_compare_three_way(a1, a1+9, a2, a2+9) == (i <=> 7)
    && std::lexicographical_compare_three_way(a1, a1+7, a2, a2+7) == 0
    && std::lexicographical_compare_three_way(a1, a1+8, a2, a2+7) > 0
    && std::lexicographical_compare_three_way(a1, a1+7, a2, a2+8) < 0;
}

static_assert( test01(~7) );
static_assert( test01(7) );
static_assert( test01(8) );

constexpr bool
test02(unsigned char i)
{
  unsigned char a1[] = { 0, 1, 2, 3, 4, 5, 6, i, 8 };
  unsigned char a2[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };

  return std::lexicographical_compare_three_way(a1, a1+0, a2, a2+0) == 0
    && std::lexicographical_compare_three_way(a1, a1+9, a2, a2+9) == (i <=> 7)
    && std::lexicographical_compare_three_way(a1, a1+7, a2, a2+7) == 0
    && std::lexicographical_compare_three_way(a1, a1+8, a2, a2+7) > 0
    && std::lexicographical_compare_three_way(a1, a1+7, a2, a2+8) < 0;
}

static_assert( test02(248) );
static_assert( test02(7) );
static_assert( test02(8) );

constexpr bool
test03(unsigned char* p)
{
  unsigned char a[2] = { 1, 2 };
  return std::lexicographical_compare_three_way(p, p, a, a+2) < 0
    && std::lexicographical_compare_three_way(a, a+2, p, p) > 0;
}

static_assert( test03(nullptr) ); // ensure memcmp not called with nullptr

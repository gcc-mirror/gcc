// { dg-do compile { target c++17 } }
// { dg-add-options no_pch }

// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

#ifndef __cpp_lib_array_constexpr
# error "Feature test macro for array constexpr is missing in <array>"
#elif __cpp_lib_array_constexpr < 201603L
# error "Feature test macro for array constexpr has wrong value in <array>"
#endif

constexpr std::size_t test01()
{
  // array
  typedef std::array<std::size_t, 6> array_type;
  array_type a = { { 0, 55, 66, 99, 4115, 2 } };
  auto v1  = a[1];
  auto v2  = a.at(2);
  auto v3  = a.front();
  auto v4  = a.back();
  auto v5 = *a.data();
  return v1 + v2 + v3 + v4 + v5;
}

static_assert( test01() == (55 + 66 + 0 + 2) );

constexpr std::size_t test02()
{
  // const array
  typedef std::array<std::size_t, 6> array_type;
  const array_type a = { { 0, 55, 66, 99, 4115, 2 } };
  auto v1  = a[1];
  auto v2  = a.at(2);
  auto v3  = a.front();
  auto v4  = a.back();
  auto v5 = *a.data();
  return v1 + v2 + v3 + v4 + v5;
}

static_assert( test02() == (55 + 66 + 0 + 2) );

constexpr bool test_zero()
{
  // zero-sized array (PR libstdc++/108258)
  std::array<int, 0> a{};
  auto v4 = a.data();
  // The standard says this is unspecified, it's null for our implementation:
  return a.data() == nullptr;
}

static_assert( test_zero() );

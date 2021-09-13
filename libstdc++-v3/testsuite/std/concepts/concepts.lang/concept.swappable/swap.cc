// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

#include <concepts>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::swap));

namespace nu
{
  struct S { bool swapped = false; };
  constexpr void swap(S& l, S& r) { l.swapped = r.swapped = true; }
  struct T { int i; };

  union U { char c; int i; };
  constexpr void swap(U& l, U& r) { l.i = r.i = 99; }
}

constexpr bool check_struct_with_adl_swap(int)
{
  nu::S s1, s2;
  std::ranges::swap(s1, s2);
  return s1.swapped && s2.swapped;
}

static_assert(check_struct_with_adl_swap(1));

constexpr bool check_array_with_adl_swap(int)
{
  nu::S s1[2], s2[2];
  std::ranges::swap(s1, s2);
  return s1[0].swapped && s2[0].swapped && s1[1].swapped && s2[1].swapped;
}

static_assert(check_array_with_adl_swap(1));

constexpr bool check_struct_without_adl_swap(int i)
{
  nu::T t1{i}, t2{2*i};
  std::ranges::swap(t1, t2);
  return t1.i == 2*i && t2.i == i;
}

static_assert(check_struct_without_adl_swap(1));

constexpr bool check_array_without_adl_swap(int i)
{
  nu::T t1[2]{i, 2*i}, t2[2]{3*i, 4*i};
  std::ranges::swap(t1, t2);
  return t1[0].i == 3*i && t2[0].i == i && t1[1].i == 4*i && t2[1].i == 2*i;
}

static_assert(check_array_without_adl_swap(1));


constexpr bool check_union_with_adl_swap(int i)
{
  nu::U u1{}, u2{};
  u1.i = u2.i = i;
  std::ranges::swap(u1, u2);
  return u1.i == 99 && u2.i == 99;
}

static_assert(check_union_with_adl_swap(1));

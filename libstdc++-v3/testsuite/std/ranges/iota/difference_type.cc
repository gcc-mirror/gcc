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

// { dg-do run { target c++20 } }

#include <ranges>
#include <limits>
#include <testsuite_hooks.h>

void
test01()
{
  using I = unsigned long long;
  auto imax = std::numeric_limits<I>::max();
  std::ranges::iota_view<I, I> i(0, imax);
  auto begin = i.begin();
  static_assert( std::input_or_output_iterator<decltype(begin)> );
  auto size = std::ranges::end(i) - std::ranges::begin(i);
  VERIFY( size > 0 );
  VERIFY( size == imax );
}

void
test02()
{
#if !defined(__STRICT_ANSI__) && __SIZEOF_INT128__
  using I = unsigned __int128;
  auto imax = std::numeric_limits<I>::max();
  std::ranges::iota_view<I, I> i(0, imax);
  auto begin = i.begin();
  static_assert( std::input_or_output_iterator<decltype(begin)> );
  auto size = std::ranges::end(i) - std::ranges::begin(i);
  VERIFY( size > 0 );
  VERIFY( size == imax );
#endif
}

int
main()
{
  test01();
  test02();
}

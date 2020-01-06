// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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
// { dg-do run { target c++2a } }

#include <ranges>
#include <testsuite_hooks.h>

void
test01()
{
  std::ranges::single_view s{4};
  static_assert(std::same_as<std::ranges::range_value_t<decltype(s)>, int>);
  static_assert(std::ranges::size(s) == 1);

  int count = 0;
  for (auto i : s)
    ++count;
  VERIFY(count == 1);
  VERIFY(*std::ranges::begin(s) == 4);
}

void
test02()
{
  std::ranges::single_view<long> s2;
  static_assert(std::same_as<std::ranges::range_value_t<decltype(s2)>, long>);
  static_assert(std::ranges::size(s2) == 1);

  int count = 0;
  for (auto l : s2)
    ++count;
  VERIFY(count == 1);
  VERIFY(*std::ranges::begin(s2) == 0L);
}

void
test03()
{
  auto s3 = std::ranges::views::single('a');
  static_assert(std::same_as<std::ranges::range_value_t<decltype(s3)>, char>);
  static_assert(std::ranges::size(s3) == 1);
  VERIFY(*std::ranges::begin(s3) == 'a');
}

int main()
{
  test01();
  test02();
  test03();
}

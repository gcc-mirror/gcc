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

// { dg-do compile { target c++20 } }

#include <ranges>
#include <span>
#include <string_view>
#include <experimental/string_view>
#include <vector>
#include <vector>
#include <set>
#include <unordered_set>
#include <regex>
#include <testsuite_iterators.h>

static_assert(std::ranges::view<std::span<int>>);
static_assert(std::ranges::view<std::span<int, 0>>);
static_assert(std::ranges::view<std::span<int, 1>>); // Changed with P2325R3
static_assert(std::ranges::view<std::string_view>);
static_assert(std::ranges::view<std::experimental::string_view>);

static_assert(!std::ranges::view<std::vector<int>>);
static_assert(!std::ranges::view<const std::vector<int>>);
static_assert(!std::ranges::view<std::initializer_list<int>>);
static_assert(!std::ranges::view<const std::initializer_list<int>>);
static_assert(!std::ranges::view<std::set<int>>);
static_assert(!std::ranges::view<const std::set<int>>);
static_assert(!std::ranges::view<std::multiset<int>>);
static_assert(!std::ranges::view<std::unordered_set<int>>);
static_assert(!std::ranges::view<std::unordered_multiset<int>>);
static_assert(!std::ranges::view<std::cmatch>);

// const test_random_access_range<T> is not a range:
static_assert(!std::ranges::view<__gnu_test::test_random_access_range<int>>);

template<typename T>
struct test_view
: __gnu_test::test_random_access_range<T>, std::ranges::view_base
{ };

static_assert(std::ranges::view<test_view<int>>);

template<>
constexpr bool std::ranges::enable_view<test_view<long>> = false;

static_assert(!std::ranges::view<test_view<long>>);

void
test01()
{
  // Verify LWG 3549 changes to ranges::enable_view.
  using std::ranges::view_interface;

  struct v1
    : __gnu_test::test_random_access_range<int>, view_interface<v1> { };
  static_assert(!std::derived_from<v1, std::ranges::view_base>);
  static_assert(std::ranges::enable_view<v1>);

  struct v2 : v1, view_interface<v2> { };
  static_assert(!std::derived_from<v2, std::ranges::view_base>);
  static_assert(!std::ranges::enable_view<v2>);

  struct v3 : __gnu_test::test_random_access_range<int> { };
  static_assert(!std::derived_from<v3, std::ranges::view_base>);
  static_assert(!std::ranges::enable_view<v3>);

  struct v4 { };
  static_assert(!std::ranges::enable_view<view_interface<v4>>);
}

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
// { dg-do compile { target c++2a } }

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
static_assert(!std::ranges::view<std::span<int, 1>>);
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
{
  // views must be default-initializable:
  test_view() : __gnu_test::test_random_access_range<T>(nullptr, nullptr) { }
};

static_assert(std::ranges::view<test_view<int>>);

template<>
constexpr bool std::ranges::enable_view<test_view<long>> = false;

static_assert(!std::ranges::view<test_view<long>>);

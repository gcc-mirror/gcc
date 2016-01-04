// { dg-options "-std=gnu++11" }
// { dg-do compile }

// Copyright (C) 2015-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <type_traits>
#include <testsuite_tr1.h>
#include <utility>
#include <array>
#include <tuple>
#include <queue>
#include <stack>

namespace funny {
  struct F {};
  void swap(F&, F&) = delete;
}
void test01()
{
  using std::__is_nothrow_swappable;
  using std::__is_swappable_impl::__is_swappable;
  using namespace __gnu_test;
  // Positive tests.
  static_assert(test_property<__is_swappable, int>(true), "");
  static_assert(test_property<__is_nothrow_swappable, int>(true), "");
  static_assert(test_property<__is_nothrow_swappable, int[1]>(true), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::pair<int, int>>(true), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::tuple<int>>(true), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::array<int, 1>>(true), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::queue<int>>(true), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::priority_queue<int>>(true), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::stack<int>>(true), "");
  // Negative tests.
  static_assert(test_property<__is_swappable, construct::DelCopy>(false), "");
  static_assert(test_property<__is_swappable, funny::F>(false), "");
  static_assert(test_property<__is_swappable, funny::F[1]>(false), "");
  static_assert(test_property<__is_nothrow_swappable,
		ThrowCopyConsClass>(false), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::pair<ThrowCopyConsClass, ThrowCopyConsClass>>(false), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::tuple<ThrowCopyConsClass>>(false), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::array<ThrowCopyConsClass, 1>>(false), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::queue<ThrowCopyConsClass>>(false), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::priority_queue<ThrowCopyConsClass>>(false), "");
  static_assert(test_property<__is_nothrow_swappable,
		std::stack<ThrowCopyConsClass>>(false), "");
}

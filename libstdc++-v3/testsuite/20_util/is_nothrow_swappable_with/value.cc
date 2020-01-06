// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

#ifndef __cpp_lib_is_swappable
# error "Feature-test macro for is_nothrow_swappable_with missing"
#elif __cpp_lib_is_swappable != 201603
# error "Feature-test macro for is_nothrow_swappable_with has wrong value"
#endif

namespace funny {
  struct T0 {};

  void swap(T0, T0) noexcept;

  struct T1
  {
    friend void swap(T1, T1) noexcept;
  };

  struct T2 {};
  struct T3 {};

  void swap(T2, T3) noexcept;
  void swap(T3, T2) noexcept;

  struct T4 { operator T0() const noexcept; };

  struct F0 {};

  void swap(F0, F0) = delete;

  struct F1 {};

  void swap(F1, F1);

  struct F2 {};

  void swap(F0, F2) noexcept;
  void swap(F2, F0);

  struct F3
  {
    friend void swap(F3, F3) = delete;
  };

  struct F4
  {
    friend void swap(F4, F4);
  };

  struct F5 { operator T0() const; };

  struct BoolLike {};

  void swap(BoolLike, bool&) noexcept;
  void swap(bool&, BoolLike) noexcept;

  struct BoolLikeErr {};

  void swap(BoolLikeErr, bool&);
  void swap(bool&, BoolLikeErr) noexcept;
}

void test01()
{
  using std::is_nothrow_swappable_with;
  using namespace __gnu_test;
  // Positive tests.
  static_assert(test_property<is_nothrow_swappable_with, int&, int&>(true),
    "");
  static_assert(test_property<is_nothrow_swappable_with, funny::T0,
    funny::T0>(true), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::T0,
    const funny::T0>(true), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::T1,
    funny::T1>(true), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::T1,
    const funny::T1>(true), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::T2,
    funny::T3>(true), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::T3,
    funny::T2>(true), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::T0,
    funny::T4>(true), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::T4,
    funny::T0>(true), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::BoolLike,
    bool&>(true), "");
  static_assert(test_property<is_nothrow_swappable_with, const funny::BoolLike,
    bool&>(true), "");

  // Negative tests.
  static_assert(test_property<is_nothrow_swappable_with, const int&,
    const int&>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, int&,
    unsigned&>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F0,
    funny::F0>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F0,
    const funny::F0>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F1,
    funny::F1>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F1,
    const funny::F1>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F0,
    funny::F2>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F2,
    funny::F0>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F3,
    funny::F3>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F3,
    const funny::F3>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F4,
    funny::F4>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F4,
    const funny::F4>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::T0,
    funny::F5>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::F5,
    funny::T0>(false), "");
  static_assert(test_property<is_nothrow_swappable_with, funny::BoolLikeErr,
    bool&>(false), "");
  static_assert(test_property<is_nothrow_swappable_with,
    const funny::BoolLikeErr, bool&>(false), "");
}

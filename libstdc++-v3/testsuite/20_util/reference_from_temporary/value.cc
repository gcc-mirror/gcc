// Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <type_traits>

#ifndef __cpp_lib_reference_from_temporary
# error "Feature test macro for reference_from_temporary is missing in <type_traits>"
#elif __cpp_lib_reference_from_temporary < 202202L
# error "Feature test macro for reference_from_temporary has wrong value in <type_traits>"
#endif

#include <testsuite_tr1.h>

void test01()
{
  using std::reference_constructs_from_temporary;
  using std::reference_converts_from_temporary;
  using namespace __gnu_test;

  struct A { A(); };

  struct B {
    operator int();
    explicit operator int&&();
  };

  struct C {
    operator int();
    explicit operator int&();
  };

  static_assert(test_property<reference_constructs_from_temporary, int, int>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&, void>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&, const volatile void>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, void, void>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&, int>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&, int&>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&, int&&>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&, long>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&, long&>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&, long&&>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, const int&, int>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, const int&, int&>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, const int&, int&&>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, const int&, long>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, const int&, long&>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, const int&, long&&>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, int&&, int>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, int&&, int&>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&&, int&&>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&&, long>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, int&&, long&>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, int&&, long&&>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, const A&, A>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, const A&, A&&>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, A&&, A>(true), "");
  static_assert(test_property<reference_constructs_from_temporary, int&, int[]>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, const int&, int[]>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, int&&, int[]>(false), "");

  static_assert(test_property<reference_converts_from_temporary, int, int>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&, void>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&, const volatile void>(false), "");
  static_assert(test_property<reference_converts_from_temporary, void, void>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&, int>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&, int&>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&, int&&>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&, long>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&, long&>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&, long&&>(false), "");
  static_assert(test_property<reference_converts_from_temporary, const int&, int>(true), "");
  static_assert(test_property<reference_converts_from_temporary, const int&, int&>(false), "");
  static_assert(test_property<reference_converts_from_temporary, const int&, int&&>(false), "");
  static_assert(test_property<reference_converts_from_temporary, const int&, long>(true), "");
  static_assert(test_property<reference_converts_from_temporary, const int&, long&>(true), "");
  static_assert(test_property<reference_converts_from_temporary, const int&, long&&>(true), "");
  static_assert(test_property<reference_converts_from_temporary, int&&, int>(true), "");
  static_assert(test_property<reference_converts_from_temporary, int&&, int&>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&&, int&&>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&&, long>(true), "");
  static_assert(test_property<reference_converts_from_temporary, int&&, long&>(true), "");
  static_assert(test_property<reference_converts_from_temporary, int&&, long&&>(true), "");
  static_assert(test_property<reference_converts_from_temporary, const A&, A>(true), "");
  static_assert(test_property<reference_converts_from_temporary, const A&, A&&>(false), "");
  static_assert(test_property<reference_converts_from_temporary, A&&, A>(true), "");
  static_assert(test_property<reference_converts_from_temporary, int&, int[]>(false), "");
  static_assert(test_property<reference_converts_from_temporary, const int&, int[]>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&&, int[]>(false), "");

  static_assert(test_property<reference_constructs_from_temporary, int&&, B>(false), "");
  static_assert(test_property<reference_constructs_from_temporary, const int&, C>(false), "");
  static_assert(test_property<reference_converts_from_temporary, int&&, B>(true), "");
  static_assert(test_property<reference_converts_from_temporary, const int&, C>(true), "");
}

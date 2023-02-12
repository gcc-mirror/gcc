// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

#ifndef __cpp_lib_bounded_array_traits
# error "Feature test macro for is_unbounded_array is missing"
#elif __cpp_lib_bounded_array_traits < 201902L
# error "Feature test macro for is_unbounded_array has wrong value"
#endif

void test01()
{
  using std::is_unbounded_array;
  using namespace __gnu_test;

  static_assert(test_category<is_unbounded_array, int[2]>(false), "");
  static_assert(test_category<is_unbounded_array, int[]>(true), "");
  static_assert(test_category<is_unbounded_array, int[2][3]>(false), "");
  static_assert(test_category<is_unbounded_array, int[][3]>(true), "");
  static_assert(test_category<is_unbounded_array, float*[2]>(false), "");
  static_assert(test_category<is_unbounded_array, float*[]>(true), "");
  static_assert(test_category<is_unbounded_array, float*[2][3]>(false), "");
  static_assert(test_category<is_unbounded_array, float*[][3]>(true), "");
  static_assert(test_category<is_unbounded_array, ClassType[2]>(false), "");
  static_assert(test_category<is_unbounded_array, ClassType[]>(true), "");
  static_assert(test_category<is_unbounded_array, ClassType[2][3]>(false), "");
  static_assert(test_category<is_unbounded_array, ClassType[][3]>(true), "");
  static_assert(test_category<is_unbounded_array, IncompleteClass[2][3]>(false), "");
  static_assert(test_category<is_unbounded_array, IncompleteClass[][3]>(true), "");
  static_assert(test_category<is_unbounded_array, int(*)[2]>(false), "");
  static_assert(test_category<is_unbounded_array, int(*)[]>(false), "");
  static_assert(test_category<is_unbounded_array, int(&)[2]>(false), "");
  static_assert(test_category<is_unbounded_array, int(&)[]>(false), "");

  // Sanity check.
  static_assert(test_category<is_unbounded_array, ClassType>(false), "");
  static_assert(test_category<is_unbounded_array, IncompleteClass>(false), "");
  static_assert(test_category<is_unbounded_array, IncompleteUnion>(false), "");
}

template <class... T> void pos()
{
  static_assert((std::is_unbounded_array_v<T> &&...));
}

template <class... T> void neg()
{
  static_assert((!std::is_unbounded_array_v<T> &&...));
}

void test02()
{
  using namespace __gnu_test;
  pos<int[], int[][3], float*[], float*[][3], ClassType[],
      ClassType[][3]>();
  neg<int[2], int[2][3], float*[2], float*[2][3], ClassType[2],
      ClassType[2][3], int(*)[2], int(&)[], int(*)[2], int(&)[], ClassType>();
}

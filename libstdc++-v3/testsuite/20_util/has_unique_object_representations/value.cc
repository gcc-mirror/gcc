// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

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

void test01()
{
  using std::has_unique_object_representations;
  using __gnu_test::test_category;

  // Positive tests.
  static_assert(test_category<has_unique_object_representations,
		char>(true), "");
  static_assert(test_category<has_unique_object_representations,
		unsigned char>(true), "");
  static_assert(test_category<has_unique_object_representations,
		signed char>(true), "");
  static_assert(test_category<has_unique_object_representations,
		unsigned>(true), "");
  static_assert(test_category<has_unique_object_representations,
                bool>(true), "");

  enum E : unsigned { };
  static_assert(test_category<has_unique_object_representations,
		E>(true), "");

  static_assert(test_category<has_unique_object_representations,
		unsigned[3]>(true), "");
  static_assert(test_category<has_unique_object_representations,
		unsigned[3][2]>(true), "");
  static_assert(test_category<has_unique_object_representations,
		unsigned[]>(true), "");
  static_assert(test_category<has_unique_object_representations,
		unsigned[][2]>(true), "");
  static_assert(test_category<has_unique_object_representations,
		E[3]>(true), "");
  static_assert(test_category<has_unique_object_representations,
		E[3][2]>(true), "");
  static_assert(test_category<has_unique_object_representations,
		E[]>(true), "");
  static_assert(test_category<has_unique_object_representations,
		E[][2]>(true), "");

  struct Padded {
    char c1;
    alignas(4) char c2;
  };

  struct Bitfield {
    int i : 3;
  };

  struct Aligned {
    alignas(4) char c;
  };

  // Negative tests.
  static_assert(test_category<has_unique_object_representations,
		void>(false), "");
  static_assert(test_category<has_unique_object_representations,
                float>(false), ""); // implementation-defined
  static_assert(test_category<has_unique_object_representations,
		Padded>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Padded[2]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Padded[2][1]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Padded[]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Padded[][1]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Bitfield>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Bitfield[2]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Bitfield[2][1]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Bitfield[]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Bitfield[][1]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Aligned>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Aligned[2]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Aligned[2][1]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Aligned[]>(false), "");
  static_assert(test_category<has_unique_object_representations,
		Aligned[][1]>(false), "");
}

void
test02()
{
  using std::has_unique_object_representations;
  using std::has_unique_object_representations_v;

  static_assert(has_unique_object_representations_v<int>
		== has_unique_object_representations<int>::value);
  static_assert(has_unique_object_representations_v<void>
		== has_unique_object_representations<void>::value);
  static_assert(has_unique_object_representations_v<float>
		== has_unique_object_representations<float>::value);
}

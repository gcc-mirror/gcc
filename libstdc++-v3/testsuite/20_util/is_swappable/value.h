// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

#if defined(test_std_is_swappable)
#  ifndef __cpp_lib_is_swappable
#   error "Feature-test macro for is_swappable missing"
#  elif __cpp_lib_is_swappable != 201603
#   error "Feature-test macro for is_swappable has wrong value"
#  endif
// Test std::is_swappable:
template<class T>
using is_swappable = std::is_swappable<T>;
#elif defined(test_std_is_swappable_ext)
// Test our __is_swappable extension:
template<class T>
using is_swappable = std::__is_swappable<T>;
#else
# error "Either test_std_is_swappable or test_std_is_swappable_ext" \
        "need to be defined"
#endif

namespace funny {
  struct F {};
  void swap(F&, F&) = delete;
  void swap(F(&)[5], F(&)[5]);

  struct F2
  {
    friend void swap(F2&, F2&) = delete;
  };

  struct F3
  {
    friend void swap(F3&, F3) {}
  };
}
void test01()
{
  using namespace __gnu_test;
  // Positive tests.
  static_assert(test_property<is_swappable, int>(true), "");
  static_assert(test_property<is_swappable, bool>(true), "");
  static_assert(test_property<is_swappable, decltype(nullptr)>(true), "");
  static_assert(test_property<is_swappable, int&>(true), "");
  static_assert(test_property<is_swappable, int&&>(true), "");
  static_assert(test_property<is_swappable, int[1]>(true), "");
  static_assert(test_property<is_swappable, int[1][2]>(true), "");
  static_assert(test_property<is_swappable, int[1][2][3]>(true), "");
  static_assert(test_property<is_swappable, int(&)[1]>(true), "");
  static_assert(test_property<is_swappable, funny::F[5]>(true), "");
  static_assert(test_property<is_swappable, funny::F3>(true), "");
  static_assert(test_property<is_swappable, funny::F3[1]>(true), "");
  static_assert(test_property<is_swappable, funny::F3[1][2]>(true), "");
  static_assert(test_property<is_swappable, funny::F3[1][2][3]>(true), "");
  static_assert(test_property<is_swappable,
    ThrowCopyConsClass>(true), "");
  static_assert(test_property<is_swappable, EnumType>(true), "");
  static_assert(test_property<is_swappable, PODType>(true), "");
  static_assert(test_property<is_swappable, UnionType>(true), "");
  static_assert(test_property<is_swappable, construct::SE>(true), "");
  static_assert(test_property<is_swappable, construct::Empty>(true), "");
  static_assert(test_property<is_swappable, void*>(true), "");
  static_assert(test_property<is_swappable, int const*>(true), "");
  static_assert(test_property<is_swappable, ClassType*>(true), "");
  static_assert(test_property<is_swappable, int ClassType::*>(true), "");
  static_assert(test_property<is_swappable,
    void (ClassType::*)()>(true), "");
  static_assert(test_property<is_swappable,
    construct::Nontrivial>(true), "");
  static_assert(test_property<is_swappable, construct::Any>(true), "");
  static_assert(test_property<is_swappable, construct::nAny>(true), "");
  static_assert(test_property<is_swappable,
		std::pair<int, int>>(true), "");
  static_assert(test_property<is_swappable,
		std::pair<int, int>[1]>(true), "");
  static_assert(test_property<is_swappable,
		std::pair<int, int>[1][2]>(true), "");
  static_assert(test_property<is_swappable,
		std::pair<int, int>[1][2][3]>(true), "");
  static_assert(test_property<is_swappable,
		std::pair<construct::Nontrivial, construct::Nontrivial>>(true), "");
  static_assert(test_property<is_swappable,
		std::tuple<int>>(true), "");
  static_assert(test_property<is_swappable,
		std::tuple<int>[1]>(true), "");
  static_assert(test_property<is_swappable,
		std::tuple<int>[1][2]>(true), "");
  static_assert(test_property<is_swappable,
		std::tuple<int>[1][2][3]>(true), "");
  static_assert(test_property<is_swappable,
		std::tuple<>>(true), "");
  static_assert(test_property<is_swappable,
		std::tuple<>[1]>(true), "");
  static_assert(test_property<is_swappable,
		std::tuple<>[1][2]>(true), "");
  static_assert(test_property<is_swappable,
		std::tuple<>[1][2][3]>(true), "");
  static_assert(test_property<is_swappable,
		std::tuple<construct::Nontrivial>>(true), "");
  static_assert(test_property<is_swappable,
		std::array<int, 1>>(true), "");
  static_assert(test_property<is_swappable,
		std::array<int, 1>[1]>(true), "");
  static_assert(test_property<is_swappable,
		std::array<int, 1>[1][2]>(true), "");
  static_assert(test_property<is_swappable,
		std::array<int, 1>[1][2][3]>(true), "");
  static_assert(test_property<is_swappable,
		std::array<construct::Nontrivial, 1>>(true), "");
  static_assert(test_property<is_swappable,
        std::array<int, 0>>(true), "");
  static_assert(test_property<is_swappable,
        std::array<construct::DelCopy, 0>>(true), "");
  static_assert(test_property<is_swappable,
		std::queue<int>>(true), "");
  static_assert(test_property<is_swappable,
		std::queue<int>[1]>(true), "");
  static_assert(test_property<is_swappable,
		std::queue<int>[1][2]>(true), "");
  static_assert(test_property<is_swappable,
		std::queue<int>[1][2][3]>(true), "");
  static_assert(test_property<is_swappable,
		std::queue<construct::Nontrivial>>(true), "");
  static_assert(test_property<is_swappable,
		std::priority_queue<int>>(true), "");
  static_assert(test_property<is_swappable,
		std::priority_queue<int>[1]>(true), "");
  static_assert(test_property<is_swappable,
		std::priority_queue<int>[1][2]>(true), "");
  static_assert(test_property<is_swappable,
		std::priority_queue<int>[1][2][3]>(true), "");
  static_assert(test_property<is_swappable,
		std::priority_queue<construct::Nontrivial>>(true), "");
  static_assert(test_property<is_swappable,
		std::stack<int>>(true), "");
  static_assert(test_property<is_swappable,
		std::stack<int>[1]>(true), "");
  static_assert(test_property<is_swappable,
		std::stack<int>[1][2]>(true), "");
  static_assert(test_property<is_swappable,
		std::stack<int>[1][2][3]>(true), "");
  static_assert(test_property<is_swappable,
		std::stack<construct::Nontrivial>>(true), "");
  // Negative tests.
  static_assert(test_property<is_swappable, void>(false), "");
  static_assert(test_property<is_swappable, const void>(false), "");
  static_assert(test_property<is_swappable, void()>(false), "");
  static_assert(test_property<is_swappable, void() const>(false), "");
  static_assert(test_property<is_swappable, void() volatile>(false), "");
  static_assert(test_property<is_swappable,
    void() const volatile>(false), "");
  static_assert(test_property<is_swappable, const int>(false), "");
  static_assert(test_property<is_swappable, const bool>(false), "");
  static_assert(test_property<is_swappable, int[]>(false), "");
  static_assert(test_property<is_swappable, const int[]>(false), "");
  static_assert(test_property<is_swappable, int[][1]>(false), "");
  static_assert(test_property<is_swappable, const int[1]>(false), "");
  static_assert(test_property<is_swappable, const int[1][2]>(false), "");
  static_assert(test_property<is_swappable, const int[1][2][3]>(false), "");
  static_assert(test_property<is_swappable, construct::DelCopy>(false), "");
  static_assert(test_property<is_swappable,
    construct::Abstract>(false), "");
  static_assert(test_property<is_swappable,
    construct::NontrivialUnion>(false), "");
  static_assert(test_property<is_swappable, funny::F>(false), "");
  static_assert(test_property<is_swappable, funny::F[1]>(false), "");
  static_assert(test_property<is_swappable, funny::F[1][2]>(false), "");
  static_assert(test_property<is_swappable, funny::F[1][2][3]>(false), "");
  static_assert(test_property<is_swappable, funny::F[4]>(false), "");
  static_assert(test_property<is_swappable, construct::DelCopy>(false), "");
  static_assert(test_property<is_swappable,
     DeletedCopyAssignClass>(false), "");
  static_assert(test_property<is_swappable,
     DeletedMoveAssignClass>(false), "");
  static_assert(test_property<is_swappable, funny::F2>(false), "");
  static_assert(test_property<is_swappable, funny::F2[1]>(false), "");
  static_assert(test_property<is_swappable, funny::F2[1][2]>(false), "");
  static_assert(test_property<is_swappable, funny::F2[1][2][3]>(false), "");
  static_assert(test_property<is_swappable, funny::F2[4]>(false), "");
  static_assert(test_property<is_swappable, funny::F2[5]>(false), "");
}

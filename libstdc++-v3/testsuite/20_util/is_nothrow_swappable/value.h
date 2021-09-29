// Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

#if defined(test_std_is_nothrow_swappable)
#  ifndef __cpp_lib_is_swappable
#   error "Feature-test macro for is_nothrow_swappable missing"
#  elif __cpp_lib_is_swappable != 201603
#   error "Feature-test macro for is_nothrow_swappable has wrong value"
#  endif
// Test std::is_nothrow_swappable:
template<class T>
using is_nothrow_swappable = std::is_nothrow_swappable<T>;
#elif defined(test_std_is_nothrow_swappable_ext)
// Test our __is_nothrow_swappable extension:
template<class T>
using is_nothrow_swappable = std::__is_nothrow_swappable<T>;
#else
# error "Either test_std_is_nothrow_swappable or " \
        "test_std_is_nothrow_swappable_ext need to be defined"
#endif

namespace funny {
  struct F {};
  void swap(F&, F&) = delete;
  void swap(F(&)[5], F(&)[5]) noexcept;
  void swap(F(&)[6], F(&)[6]);
  struct A {};
  void swap(A&, A&) noexcept(false);
}
namespace std {
  template<>
  void swap<funny::A>(funny::A&, funny::A&) noexcept
  {
  }

  template<>
  void swap<funny::A>(funny::A(&)[3], funny::A(&)[3]) noexcept(false)
  {
  }
}
namespace ns1 {
  struct SwapThrow {};
  void swap(SwapThrow&, SwapThrow&);
  void swap(SwapThrow(&)[3], SwapThrow(&)[3]) noexcept;
}

namespace ns2 {
  struct SwapThrow {
    SwapThrow() noexcept = default;
    SwapThrow(const SwapThrow&) noexcept(false);
    SwapThrow& operator=(const SwapThrow&) noexcept(false);
  };
}

namespace ns3 {
  struct SwapNoThrow {
    SwapNoThrow() noexcept = default;
    SwapNoThrow(const SwapNoThrow&) noexcept(false);
    SwapNoThrow& operator =(const SwapNoThrow&) noexcept(false);
  };
  void swap(SwapNoThrow&, SwapNoThrow&) noexcept;
}

namespace ns4 {
  struct SwapNoThrow {};
}

namespace ns5 {
  struct SwapThrow {
    SwapThrow() noexcept = default;
    SwapThrow(SwapThrow&&) noexcept;
    SwapThrow& operator=(const SwapThrow&) noexcept(false);
  };
}

namespace comps {
  struct CompareNoThrowCopyable
  {
    template<class T>
    bool operator()(const T&, const T&) const
    { return false; }
  };

  struct CompareNonCopyable
  {
    CompareNonCopyable() = default;
    CompareNonCopyable(const CompareNonCopyable&) = delete;
    CompareNonCopyable& operator=(const CompareNonCopyable&) noexcept;

    template<class T>
    bool operator()(const T&, const T&) const
    { return false; }
  };

  struct CompareThrowCopyable
  {
    CompareThrowCopyable() = default;
    CompareThrowCopyable(const CompareThrowCopyable&) noexcept(false);
    CompareThrowCopyable& operator=(const CompareThrowCopyable&);

    template<class T>
    bool operator()(const T&, const T&) const
    { return false; }
  };
}

void test01()
{
  using namespace __gnu_test;
  // Positive tests.
  static_assert(test_property<is_nothrow_swappable, int>(true), "");
  static_assert(test_property<is_nothrow_swappable, bool>(true), "");
  static_assert(test_property<is_nothrow_swappable,
    decltype(nullptr)>(true), "");
  static_assert(test_property<is_nothrow_swappable, int&>(true), "");
  static_assert(test_property<is_nothrow_swappable, int&&>(true), "");
  static_assert(test_property<is_nothrow_swappable, int[1]>(true), "");
  static_assert(test_property<is_nothrow_swappable, int[1][2]>(true), "");
  static_assert(test_property<is_nothrow_swappable, int[1][2][3]>(true), "");
  static_assert(test_property<is_nothrow_swappable, funny::F[5]>(true), "");
  static_assert(test_property<is_nothrow_swappable, EnumType>(true), "");
  static_assert(test_property<is_nothrow_swappable, PODType>(true), "");
  static_assert(test_property<is_nothrow_swappable, UnionType>(true), "");
  static_assert(test_property<is_nothrow_swappable,
    construct::SE>(true), "");
  static_assert(test_property<is_nothrow_swappable,
    construct::Empty>(true), "");
  static_assert(test_property<is_nothrow_swappable, void*>(true), "");
  static_assert(test_property<is_nothrow_swappable, void(*)()>(true), "");
  static_assert(test_property<is_nothrow_swappable, int const*>(true), "");
  static_assert(test_property<is_nothrow_swappable, ClassType*>(true), "");
  static_assert(test_property<is_nothrow_swappable,
    int ClassType::*>(true), "");
  static_assert(test_property<is_nothrow_swappable,
    void (ClassType::*)()>(true), "");
  static_assert(test_property<is_nothrow_swappable,
    int (ClassType::*)() const volatile>(true), "");
  static_assert(test_property<is_nothrow_swappable,
    ns1::SwapThrow[3]>(true), "");
  static_assert(!noexcept(std::swap(std::declval<ns1::SwapThrow(&)[3]>(),
                                    std::declval<ns1::SwapThrow(&)[3]>())),
                                    "");
  static_assert(test_property<is_nothrow_swappable,
        ns3::SwapNoThrow>(true), "");
  static_assert(!noexcept(std::swap(std::declval<ns3::SwapNoThrow&>(),
                                    std::declval<ns3::SwapNoThrow&>())), "");
  static_assert(test_property<is_nothrow_swappable,
        ns3::SwapNoThrow[1]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        ns3::SwapNoThrow[3]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        ns3::SwapNoThrow[2][3][4]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        ns4::SwapNoThrow>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        ns4::SwapNoThrow[1]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        ns4::SwapNoThrow[3]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        ns4::SwapNoThrow[2][3][4]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::pair<int, int>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::pair<int, int>[1]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::pair<int, int>[1][2]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::tuple<int>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::tuple<int>[1]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::tuple<int>[1][2]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::tuple<>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::tuple<>[1]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::tuple<>[1][2]>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::array<int, 1>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::array<int, 0>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::array<construct::DelCopy, 0>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::array<ns1::SwapThrow, 0>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::queue<int>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::priority_queue<int>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::stack<int>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::priority_queue<int, std::vector<int>,
        comps::CompareNoThrowCopyable>>(true), "");

  // Negative tests.
  static_assert(test_property<is_nothrow_swappable, void>(false), "");
  static_assert(test_property<is_nothrow_swappable, const void>(false), "");
  static_assert(test_property<is_nothrow_swappable, void()>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    void() const>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    void() volatile>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    void() const volatile>(false), "");
  static_assert(test_property<is_nothrow_swappable, const int>(false), "");
  static_assert(test_property<is_nothrow_swappable, const bool>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    const int[1]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    const int[1][2]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    const int[1][2][3]>(false), "");
  static_assert(test_property<is_nothrow_swappable, int[]>(false), "");
  static_assert(test_property<is_nothrow_swappable, const int[]>(false), "");
  static_assert(test_property<is_nothrow_swappable, int[][1]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    const funny::F[5]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    construct::Abstract>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    construct::DelCopy>(false), "");
  static_assert(test_property<is_nothrow_swappable, funny::F>(false), "");
  static_assert(test_property<is_nothrow_swappable, funny::F[1]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    funny::F[1][2]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    funny::F[1][2][3]>(false), "");
  static_assert(test_property<is_nothrow_swappable, funny::F[6]>(false), "");
  static_assert(test_property<is_nothrow_swappable, funny::A>(false), "");
  static_assert(noexcept(std::swap(std::declval<funny::A&>(),
                                   std::declval<funny::A&>())), "");
  static_assert(test_property<is_nothrow_swappable, funny::A[3]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns1::SwapThrow>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns1::SwapThrow[1]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns1::SwapThrow[3][2]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns1::SwapThrow[2][3][4]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns2::SwapThrow>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns2::SwapThrow[1]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns2::SwapThrow[2][3][4]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns5::SwapThrow>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns5::SwapThrow[1]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
    ns5::SwapThrow[2][3][4]>(false), "");
  static_assert(test_property<is_nothrow_swappable,
		ThrowCopyConsClass>(false), "");
  static_assert(test_property<is_nothrow_swappable,
		std::pair<ThrowCopyConsClass, ThrowCopyConsClass>>(false), "");
  static_assert(test_property<is_nothrow_swappable,
		std::tuple<ThrowCopyConsClass>>(false), "");
  static_assert(test_property<is_nothrow_swappable,
		std::array<ThrowCopyConsClass, 1>>(false), "");
  static_assert(test_property<is_nothrow_swappable,
		std::queue<ThrowCopyConsClass>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
		std::priority_queue<ThrowCopyConsClass,
				    std::vector<ThrowCopyConsClass>,
				    comps::CompareNoThrowCopyable>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
		std::stack<ThrowCopyConsClass>>(true), "");
  static_assert(test_property<is_nothrow_swappable,
        std::priority_queue<int, std::vector<int>,
        comps::CompareNonCopyable>>(false), "");
  static_assert(test_property<is_nothrow_swappable,
        std::priority_queue<int, std::vector<int>,
        comps::CompareThrowCopyable>>(false), "");
}

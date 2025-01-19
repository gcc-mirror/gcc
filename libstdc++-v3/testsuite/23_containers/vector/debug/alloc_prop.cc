// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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
//
// { dg-do compile { target c++11 } }

#include <debug/vector>
#include <type_traits>
#include <testsuite_allocator.h>

template<typename T, typename A>
  void
  test()
  {
    typedef std::vector<T, A>         base;
    typedef __gnu_debug::vector<T, A> debug;

    using std::is_nothrow_default_constructible;
    using std::is_nothrow_copy_constructible;
    using std::is_nothrow_move_constructible;
    using std::is_nothrow_copy_assignable;
    using std::is_nothrow_move_assignable;

    static_assert(
        is_nothrow_default_constructible<base>::value
        == is_nothrow_default_constructible<debug>::value,
        "nothrow default constructible");

    static_assert(
        is_nothrow_copy_constructible<base>::value
        == is_nothrow_copy_constructible<debug>::value,
        "nothrow copy constructible");

    static_assert(
        is_nothrow_move_constructible<base>::value
        == is_nothrow_move_constructible<debug>::value,
        "nothrow move constructible");

    static_assert(
        is_nothrow_copy_assignable<base>::value
        == is_nothrow_copy_assignable<debug>::value,
        "nothrow move assignable");

    static_assert(
        is_nothrow_move_assignable<base>::value
        == is_nothrow_move_assignable<debug>::value,
        "nothrow move assignable");
  }

struct X
{
  X() { }
  ~X() { }
  X(const X&) { }
  X(X&&) { }
  X& operator=(const X&) { return *this; }
  X& operator=(X&&) { return *this; }
};

int main()
{
  using __gnu_test::propagating_allocator;
  using __gnu_test::SimpleAllocator;

  test<int, std::allocator<int>>();
  test<int, SimpleAllocator<int>>();
  test<int, propagating_allocator<int, true>>();
  test<int, propagating_allocator<int, false>>();
  test<X, std::allocator<X>>();
  test<X, SimpleAllocator<X>>();
  test<X, propagating_allocator<X, true>>();
  test<X, propagating_allocator<X, false>>();

  return 0;
}

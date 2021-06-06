// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
// { dg-do run { target c++2a } }

#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using std::same_as;

void
test01()
{
  struct R
  {
    constexpr int empty() const & { return 0; }
    constexpr const void* empty() const && { return this; }
  };
  constexpr R r;
  static_assert( !std::ranges::empty(r) );
  static_assert( same_as<decltype(std::ranges::empty(r)), bool> );
  // PR libstdc++/100824
  // ranges::empty should treat the subexpression as an lvalue
  static_assert( !std::ranges::empty(std::move(r)) );
  static_assert( same_as<decltype(std::ranges::empty(std::move(r))), bool> );
}

void
test02()
{
  using __gnu_test::test_range;
  using __gnu_test::test_sized_range;
  using __gnu_test::random_access_iterator_wrapper;
  using __gnu_test::forward_iterator_wrapper;
  using __gnu_test::input_iterator_wrapper;
  using __gnu_test::output_iterator_wrapper;

  int a[] = { 0, 1 };
  VERIFY( !std::ranges::empty(a) );

  test_range<int, random_access_iterator_wrapper> r(a);
  VERIFY( !std::ranges::empty(r) );

  test_range<int, forward_iterator_wrapper> i(a);
  VERIFY( !std::ranges::empty(i) );

  test_sized_range<int, random_access_iterator_wrapper> sr(a);
  VERIFY( !std::ranges::empty(sr) );

  test_sized_range<int, input_iterator_wrapper> si(a);
  VERIFY( !std::ranges::empty(si) );

  test_sized_range<int, output_iterator_wrapper> so(a);
  VERIFY( !std::ranges::empty(so) );
}

void
test03()
{
  // PR libstdc++/100824
  // ranges::empty should treat the subexpression as an lvalue

  struct R
  {
    constexpr bool empty() & { return true; }
  };
  static_assert( std::ranges::empty(R{}) );

  struct R2
  {
    constexpr unsigned size() & { return 0; }
  };
  static_assert( std::ranges::empty(R2{}) );
}

int
main()
{
  test01();
  test02();
  test03();
}

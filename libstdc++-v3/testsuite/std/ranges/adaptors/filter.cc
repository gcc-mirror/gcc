// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }

#include <algorithm>
#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_range;
using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;
namespace views = std::ranges::views;

void
test01()
{
  int x[] = {1,2,3,4,5,6};
  auto is_odd = [] (int i) { return i%2==1; };
  auto v = x | views::filter(is_odd);
  using R = decltype(v);
  static_assert(std::same_as<int&, decltype(*v.begin())>);
  static_assert(ranges::view<R>);
  static_assert(ranges::input_range<R>);
  static_assert(ranges::common_range<R>);
  static_assert(!ranges::sized_range<R>);
  static_assert(ranges::bidirectional_range<R>);
  static_assert(!ranges::random_access_range<R>);
  static_assert(ranges::range<views::all_t<R>>);
  VERIFY( ranges::equal(v, (int[]){1,3,5}) );
  VERIFY( ranges::equal(v | views::reverse, (int[]){5,3,1}) );
  VERIFY( v.pred()(3) == true );
  VERIFY( v.pred()(4) == false );
}

void
test02()
{
  int x[] = {1,2,3,4,5,6};
  auto f = [flag=false] (int) mutable { return flag = !flag; };
  auto v = views::filter(f)(x);
  using R = decltype(v);
  static_assert(std::same_as<int&, decltype(*v.begin())>);
  static_assert(ranges::range<R>);
  static_assert(std::copyable<R>);
  static_assert(!ranges::view<const R>);
  VERIFY( ranges::equal(v, (int[]){1,3,5}) );
}

struct X
{
  int i, j;
};

void
test03()
{
  X x[] = {{1,3}, {2,5}, {3,7}, {4,9}};
  test_range<X, bidirectional_iterator_wrapper> rx(x);
  auto v = rx | views::filter([] (auto&& p) { return p.i%2==0; });
  int sum = 0;
  for (auto i = v.begin(); i != v.end(); ++i)
    sum += i->j;
  VERIFY( sum == 14 );
}

void
test04()
{
  auto yes = [] (int) { return true; };
  VERIFY( ranges::equal(views::iota(0) | views::filter(yes) | views::take(1),
			(int[]){0}) );
}

// The following tests that filter_view::begin caches its result.

template<template<typename> typename wrapper>
struct test_view : ranges::view_base
{
  bool begin_already_called = false;
  static inline int x[] = {1,2,3,4,5};
  test_range<int, wrapper> rx{x};

  auto
  begin()
  {
    if (begin_already_called)
      x[0] = 10;
    begin_already_called = true;
    return rx.begin();
  }

  auto
  end()
  { return rx.end(); }
};

template<template<typename> typename wrapper>
void
test05()
{
  auto v = test_view<wrapper>{} | views::filter([] (int i) { return i%2 == 0; });
  VERIFY( ranges::equal(v, (int[]){2,4}) );
  VERIFY( ranges::equal(v, (int[]){2,4}) );
}

template<auto filter = views::filter>
void
test06()
{
  // Verify SFINAE behavior.
  extern int x[5];
  auto p = [] (int*) { return true; };
  static_assert(!requires { filter(); });
  static_assert(!requires { filter(x, p, p); });
  static_assert(!requires { filter(x, p); });
  static_assert(!requires { filter(p)(x); });
  static_assert(!requires { x | (filter(p) | views::all); });
  static_assert(!requires { (filter(p) | views::all)(x); });
  static_assert(!requires { filter | views::all; });
  static_assert(!requires { views::all | filter; });
}

constexpr bool
test07()
{
  struct Pred
  {
    constexpr Pred() { }
    constexpr Pred(const Pred&) { }
    constexpr Pred(Pred&&) { }
    // These make it non-copyable, so non-copyable-box<Pred> will provide
    // assignment.
    Pred& operator=(const Pred&) = delete;
    Pred& operator=(Pred&&) = delete;

    bool operator()(int i) const { return i < 10; }
  };

  int i = 0;
  ranges::filter_view v(views::single(i), Pred{});
  // LWG 3572. copyable-box should be fully constexpr
  v = v;
  v = std::move(v);

  return true;
}

static_assert( test07() );

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05<forward_iterator_wrapper>();
  test05<random_access_iterator_wrapper>();
  test06();
  test07();
}

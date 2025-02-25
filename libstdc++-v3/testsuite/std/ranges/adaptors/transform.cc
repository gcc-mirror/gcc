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
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;
namespace views = std::ranges::views;

void
test01()
{
  int x[] = {1,2,3,4,5};
  auto is_odd = [] (int i) { return i%2==1; };
  auto v = x | views::transform(is_odd);
  VERIFY( ranges::equal(v, (int[]){1,0,1,0,1}) );
  using R = decltype(v);
  static_assert(std::same_as<bool, decltype(*ranges::begin(v))>);
  static_assert(ranges::view<R>);
  static_assert(ranges::sized_range<R>);
  static_assert(ranges::random_access_range<R>);
}

struct X
{
  int i,j;
};

void
test02()
{
  X x[] = {{1,2},{3,4},{5,6},{7,8},{9,10}};
  test_range<X, random_access_iterator_wrapper> rx(x);
  auto v = rx | views::transform(&X::i);
  VERIFY( ranges::size(v) == 5 );
  VERIFY( ranges::distance(v.begin(), v.end()) == 5 );
  VERIFY( ranges::equal(v, (int[]){1,3,5,7,9}) );
  VERIFY( ranges::equal(v | views::reverse, (int[]){9,7,5,3,1}) );
  using R = decltype(v);
  static_assert(std::same_as<int&, decltype(*ranges::begin(v))>);
  static_assert(std::same_as<int, std::iter_value_t<ranges::iterator_t<R>>>);
  static_assert(ranges::view<R>);
  static_assert(ranges::sized_range<R>);
  static_assert(!ranges::common_range<R>);
  static_assert(ranges::random_access_range<R>);
}

void
test03()
{
  auto id = [] (int i) { return i; };
  auto v = views::iota(0) | (views::filter(id)
			     | views::transform(id)
			     | views::take(5));
  VERIFY( ranges::equal(v, (int[]){1,2,3,4,5}) );
}

void
test04()
{
  // LWG 3301
    {
      auto f = [] (int x) { return x; };
      int x[] = {1,2,3,4,5};
      auto v = x | views::transform(f);
      auto i = v.begin();
      using Cat = decltype(i)::iterator_category;
      static_assert(std::same_as<Cat, std::input_iterator_tag>);
    }

    {
      auto f = [] (int &x) -> int& { return x; };
      int x[] = {1,2,3,4,5};
      auto v = x | views::transform(f);
      auto i = v.begin();
      using Cat = decltype(i)::iterator_category;
      static_assert(std::derived_from<Cat, std::forward_iterator_tag>);
    }
}

void
test05()
{
  int x[] = {1,2,3,4,5};
  auto i = std::counted_iterator(x, 5);
  auto r = ranges::subrange{i, std::default_sentinel};
  auto v = r | views::transform(std::negate{});

  // Verify that _Iterator<false> is implicitly convertible to _Iterator<true>.
  static_assert(!std::same_as<decltype(ranges::begin(v)),
			      decltype(ranges::cbegin(v))>);
  auto a = ranges::cbegin(v);
  a = ranges::begin(v);

  // Verify that _Sentinel<false> is implicitly convertible to _Sentinel<true>.
  static_assert(!ranges::common_range<decltype(v)>);
  static_assert(!std::same_as<decltype(ranges::end(v)),
			      decltype(ranges::cend(v))>);
  auto b = ranges::cend(v);
  b = ranges::end(v);
}

struct Y
{
  using Iter = __gnu_test::forward_iterator_wrapper<Y>;

  friend auto operator-(Iter l, Iter r) { return l.ptr - r.ptr; }
};

void
test06()
{
  using ranges::next;
  using ranges::begin;

  // LWG 3483
  Y y[3];
  __gnu_test::test_forward_range<Y> r(y);
  auto v = views::transform(r, std::identity{});
  auto b = begin(v);
  static_assert( !ranges::random_access_range<decltype(r)> );
  static_assert( std::sized_sentinel_for<decltype(b), decltype(b)> );
  VERIFY( (next(b, 1) - b) == 1 );
  const auto v_const = v;
  auto b_const = begin(v_const);
  VERIFY( (next(b_const, 2) - b_const) == 2 );
}

void
test07()
{
  int x[] = {1,2,3,4,5};
  auto v1 = views::transform([] (auto& x) { return &x; });
  auto v2 = views::transform([] (auto x) { return *x; });
  auto v = x | (v1 | v2);
  VERIFY( ranges::equal(v, x) );
}

template<auto transform = views::transform>
void
test08()
{
  // Verify SFINAE behavior.
  extern int x[5];
  auto f = [] (int* e) { return e; };
  static_assert(!requires { transform(); });
  static_assert(!requires { transform(x, f, f); });
  static_assert(!requires { transform(x, f); });
  static_assert(!requires { transform(f)(x); });
  static_assert(!requires { x | (transform(f) | views::all); });
  static_assert(!requires { (transform(f) | views::all)(x); });
  static_assert(!requires { transform | views::all; });
  static_assert(!requires { views::all | transform; });
}

template<auto transform = views::transform>
void
test09()
{
  extern int x[5];
  struct move_only {
    move_only() { }
    move_only(move_only&&) { }
    int operator()(int i) const { return i; }
  };
#if __cpp_lib_ranges >= 202207L
  // P2494R2 Relaxing range adaptors to allow for move only types
  static_assert( requires { transform(x, move_only{}); } );
  static_assert( requires { x | transform(move_only{}); } ); // PR libstdc++/118413
#else
  static_assert( ! requires { transform(x, move_only{}); } );
  static_assert( ! requires { x | transform(move_only{}); } );
#endif
}

void
test10()
{
  struct F {
    short operator()(int) { return 0; }
    const int& operator()(const int& i) const { return i; }
  };

  int x[] {2, 4};
  const auto xform = x | views::transform(F{});
  using const_iterator = decltype(xform.begin());
  // LWG 3564. transform_view::iterator<true>::value_type and iterator_category
  // should use const F&
  static_assert(std::same_as<std::iter_value_t<const_iterator>, int>);
  using cat = std::iterator_traits<const_iterator>::iterator_category;
  static_assert(std::same_as<cat, std::random_access_iterator_tag>);
}

void
test11()
{
  struct MoveIt {
    int&& operator()(int& i) const { return std::move(i); }
  };

  int x[] {2, 4};
  auto xform = x | views::transform(MoveIt{});
  using iterator = decltype(xform.begin());
  // LWG 3798. Rvalue reference and iterator_category
  using cat = std::iterator_traits<iterator>::iterator_category;
  static_assert(std::same_as<cat, std::random_access_iterator_tag>);
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  test09();
  test10();
  test11();
}

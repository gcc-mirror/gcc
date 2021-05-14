// Copyright (C) 2021 Free Software Foundation, Inc.
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
// { dg-do compile { target c++2a } }

// Verify P2259 changes.

#include <ranges>
#include <tuple>
#include <vector>

namespace ranges = std::ranges;
namespace views = std::views;

using std::__detail::__iter_without_category;

template<typename _Range>
concept only_cxx20_input_range = ranges::input_range<_Range>
  && !ranges::forward_range<_Range>
  && __iter_without_category<ranges::iterator_t<_Range>>;

void
test01()
{
  extern std::vector<int> vec;
  only_cxx20_input_range auto v0
    = vec
    | views::transform([](int c) { return views::single(c); })
    | views::join;

  // Verify the changes to filter_view.
  only_cxx20_input_range auto v1 = v0 | views::filter([](int c) { return c > 0; });

  // Verify the changes to transform_view.
  only_cxx20_input_range auto v2 = v0 | views::transform([](int& c) -> auto& { return c; });

  // Verify the changes to split_view.
  only_cxx20_input_range auto v3 = v0 | views::split(12);
  static_assert(only_cxx20_input_range<decltype(*v3.begin())>);

  // Verify the changes to join_view.
  only_cxx20_input_range auto v4 = v0 | views::split(12) | views::join;

  // Verify the changes to elements_view.
  only_cxx20_input_range auto v5
    = v0
    | views::transform([](int c) { return std::make_tuple(c, c); })
    | views::elements<0>;

  // Verify the changes to common_iterator.
  only_cxx20_input_range auto v6 = v0 | views::common;
  *(v6.begin()++);

  // Verify the changes to iota_view.
  only_cxx20_input_range auto v8 = ranges::iota_view{v0.begin()};

  // Verify the changes to move_iterator.
  __iter_without_category auto i9 = std::make_move_iterator(v0.begin());

  // Verify the changes to counted_iterator.
  extern std::counted_iterator<int*> i10;
  static_assert(std::contiguous_iterator<decltype(i10)>);
  static_assert(std::same_as<std::iterator_traits<decltype(i10)>::iterator_category,
			     std::random_access_iterator_tag>);
  i10.operator->();
  __iter_without_category auto i11 = std::counted_iterator{v0.begin(), 5};
}

void
test02()
{
  // Verify LWG 3291 example.
  auto v = views::iota(0);
  auto i = std::counted_iterator{v.begin(), 5};
  static_assert(std::random_access_iterator<decltype(i)>);
}

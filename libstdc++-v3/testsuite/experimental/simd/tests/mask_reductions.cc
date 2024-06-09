// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// expensive: * [1-9] * *
#include "bits/main.h"

template <typename V>
  void
  test()
  {
    using M = typename V::mask_type;
    const M alternating_mask = make_alternating_mask<M>();
    COMPARE(alternating_mask[0], false); // assumption below
    auto&& gen = make_mask<M>;

    // all_of
    VERIFY(all_of(M{true}));
    VERIFY(!all_of(alternating_mask));
    VERIFY(!all_of(M{false}));
    using std::experimental::all_of;
    VERIFY(all_of(true));
    VERIFY(!all_of(false));
    VERIFY(sfinae_is_callable<bool>(
	[](auto x) -> decltype(std::experimental::all_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<int>(
	[](auto x) -> decltype(std::experimental::all_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<float>(
	[](auto x) -> decltype(std::experimental::all_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<char>(
	[](auto x) -> decltype(std::experimental::all_of(x)) { return {}; }));

    // any_of
    VERIFY(any_of(M{true}));
    COMPARE(any_of(alternating_mask), M::size() > 1);
    VERIFY(!any_of(M{false}));
    using std::experimental::any_of;
    VERIFY(any_of(true));
    VERIFY(!any_of(false));
    VERIFY(sfinae_is_callable<bool>(
	[](auto x) -> decltype(std::experimental::any_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<int>(
	[](auto x) -> decltype(std::experimental::any_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<float>(
	[](auto x) -> decltype(std::experimental::any_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<char>(
	[](auto x) -> decltype(std::experimental::any_of(x)) { return {}; }));

    // none_of
    VERIFY(!none_of(M{true}));
    COMPARE(none_of(alternating_mask), M::size() == 1);
    VERIFY(none_of(M{false}));
    using std::experimental::none_of;
    VERIFY(!none_of(true));
    VERIFY(none_of(false));
    VERIFY(sfinae_is_callable<bool>(
	[](auto x) -> decltype(std::experimental::none_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<int>(
	[](auto x) -> decltype(std::experimental::none_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<float>(
	[](auto x) -> decltype(std::experimental::none_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<char>(
	[](auto x) -> decltype(std::experimental::none_of(x)) { return {}; }));

    // some_of
    VERIFY(!some_of(M{true}));
    VERIFY(!some_of(M{false}));
    if (M::size() > 1)
      {
	VERIFY(some_of(gen({true, false})));
	VERIFY(some_of(gen({false, true})));
	if (M::size() > 3)
	  {
	    VERIFY(some_of(gen({0, 0, 0, 1})));
	  }
      }
    using std::experimental::some_of;
    VERIFY(!some_of(true));
    VERIFY(!some_of(false));
    VERIFY(sfinae_is_callable<bool>(
	[](auto x) -> decltype(std::experimental::some_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<int>(
	[](auto x) -> decltype(std::experimental::some_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<float>(
	[](auto x) -> decltype(std::experimental::some_of(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<char>(
	[](auto x) -> decltype(std::experimental::some_of(x)) { return {}; }));

    // popcount
    COMPARE(popcount(M{true}), int(M::size()));
    COMPARE(popcount(alternating_mask), int(M::size()) / 2);
    COMPARE(popcount(M{false}), 0);
    COMPARE(popcount(gen({0, 0, 1})), int(M::size()) / 3);
    COMPARE(popcount(gen({0, 0, 0, 1})), int(M::size()) / 4);
    COMPARE(popcount(gen({0, 0, 0, 0, 1})), int(M::size()) / 5);
    COMPARE(std::experimental::popcount(true), 1);
    COMPARE(std::experimental::popcount(false), 0);
    VERIFY(sfinae_is_callable<bool>(
	[](auto x) -> decltype(std::experimental::popcount(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<int>(
	[](auto x) -> decltype(std::experimental::popcount(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<float>(
	[](auto x) -> decltype(std::experimental::popcount(x)) { return {}; }));
    VERIFY(!sfinae_is_callable<char>(
	[](auto x) -> decltype(std::experimental::popcount(x)) { return {}; }));

    // find_first_set
    {
      M x(false);
      for (int i = int(M::size() / 2 - 1); i >= 0; --i)
	{
	  x[i] = true;
	  COMPARE(find_first_set(x), i) << x;
	}
      x = M(false);
      for (int i = int(M::size() - 1); i >= 0; --i)
	{
	  x[i] = true;
	  COMPARE(find_first_set(x), i) << x;
	}
    }
    COMPARE(find_first_set(M{true}), 0);
    if (M::size() > 1)
      {
	COMPARE(find_first_set(gen({0, 1})), 1);
      }
    if (M::size() > 2)
      {
	COMPARE(find_first_set(gen({0, 0, 1})), 2);
      }
    COMPARE(std::experimental::find_first_set(true), 0);
    VERIFY(sfinae_is_callable<bool>(
	[](auto x) -> decltype(std::experimental::find_first_set(x)) {
	  return {};
	}));
    VERIFY(!sfinae_is_callable<int>(
	[](auto x) -> decltype(std::experimental::find_first_set(x)) {
	  return {};
	}));
    VERIFY(!sfinae_is_callable<float>(
	[](auto x) -> decltype(std::experimental::find_first_set(x)) {
	  return {};
	}));
    VERIFY(!sfinae_is_callable<char>(
	[](auto x) -> decltype(std::experimental::find_first_set(x)) {
	  return {};
	}));

    // find_last_set
    {
      M x(false);
      for (int i = 0; i < int(M::size()); ++i)
	{
	  x[i] = true;
	  COMPARE(find_last_set(x), i) << x;
	}
    }
    COMPARE(find_last_set(M{true}), int(M::size()) - 1);
    if (M::size() > 1)
      {
	COMPARE(find_last_set(gen({1, 0})),
		int(M::size()) - 2 + int(M::size() & 1));
      }
    if (M::size() > 3 && (M::size() & 3) == 0)
      {
	COMPARE(find_last_set(gen({1, 0, 0, 0})),
		int(M::size()) - 4 - int(M::size() & 3));
      }
    COMPARE(std::experimental::find_last_set(true), 0);
    VERIFY(sfinae_is_callable<bool>(
	[](auto x) -> decltype(std::experimental::find_last_set(x)) {
	  return {};
	}));
    VERIFY(!sfinae_is_callable<int>(
	[](auto x) -> decltype(std::experimental::find_last_set(x)) {
	  return {};
	}));
    VERIFY(!sfinae_is_callable<float>(
	[](auto x) -> decltype(std::experimental::find_last_set(x)) {
	  return {};
	}));
    VERIFY(!sfinae_is_callable<char>(
	[](auto x) -> decltype(std::experimental::find_last_set(x)) {
	  return {};
	}));
  }

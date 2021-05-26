// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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
// { dg-require-normal-namespace "" }

#include <iterator>
#include "./synopsis_c++17.cc"

namespace std
{
  template<class> struct incrementable_traits;
  template<class> struct readable_traits;

  struct contiguous_iterator_tag;

  namespace ranges
  {
    // These are function objects of unspecified type.
    auto& _distance = distance;
    auto& _advance = advance;
    auto& _next = next;
    auto& _prev = prev;
  }

  template<semiregular S> class move_sentinel;

  template<input_or_output_iterator I, sentinel_for<I> S>
    requires (!same_as<I, S>) && copyable<I>
    class common_iterator;

  template<class I, class S>
    struct incrementable_traits<common_iterator<I, S>>;

  template<input_iterator I, class S>
    struct iterator_traits<common_iterator<I, S>>;

  struct default_sentinel_t;

  template<input_or_output_iterator I> class counted_iterator;

  template<class I>
    struct incrementable_traits<counted_iterator<I>>;

  template<input_iterator I>
    requires same_as<__detail::__iter_traits<I>, iterator_traits<I>>
    struct iterator_traits<counted_iterator<I>>;

  struct unreachable_sentinel_t;
}

struct I { };
template<> constexpr bool std::disable_sized_sentinel_for<I, I> = true;

namespace __gnu_test
{
  // customization points
  constexpr auto* iter_move = &std::ranges::iter_move;
  constexpr auto* iter_swap = &std::ranges::iter_swap;
  // sized sentinels
  constexpr bool const* disable_sized_sentinel_for
    = &std::disable_sized_sentinel_for<void, void>;
  // default sentinels
  constexpr std::default_sentinel_t const* default_sentinel
    = &std::default_sentinel;
  // unreachable sentinels
  constexpr std::unreachable_sentinel_t const* unreachable_sentinel
    = &std::unreachable_sentinel;
}

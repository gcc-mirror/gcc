// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#include <optional>

struct X
{
  template <typename T>
    bool operator==(const T&) /* not const */ { return false; }

  template <typename T>
    bool operator!=(const T&) /* not const */ { return false; }

  template <typename T>
    bool operator<(const T&) /* not const */ { return false; }

  template <typename T>
    bool operator>(const T&) /* not const */ { return false; }

  template <typename T>
    bool operator<=(const T&) /* not const */ { return false; }

  template <typename T>
    bool operator>=(const T&) /* not const */ { return false; }
};

void test01()
{
  // PR 96269 optional comparison with nullopt fails
  std::optional<X> x;
  bool eq [[maybe_unused]] = std::nullopt == x;

  bool ne [[maybe_unused]] = std::nullopt != x;
  bool lt [[maybe_unused]] = std::nullopt <  x;
  bool gt [[maybe_unused]] = std::nullopt >  x;
  bool le [[maybe_unused]] = std::nullopt <= x;
  bool ge [[maybe_unused]] = std::nullopt >= x;
}

template<typename T>
  concept optional_lt_cmp
    = requires(std::optional<T> o, T t) { { o < t } -> std::same_as<bool>; };

template<typename T>
  concept optional_gt_cmp
    = requires(std::optional<T> o, T t) { { o > t } -> std::same_as<bool>; };

template<typename T>
  concept optional_le_cmp
    = requires(std::optional<T> o, T t) { { o <= t } -> std::same_as<bool>; };

template<typename T>
  concept optional_ge_cmp
    = requires(std::optional<T> o, T t) { { o >= t } -> std::same_as<bool>; };

static_assert( ! optional_lt_cmp<X> );
static_assert( ! optional_gt_cmp<X> );
static_assert( ! optional_le_cmp<X> );
static_assert( ! optional_ge_cmp<X> );

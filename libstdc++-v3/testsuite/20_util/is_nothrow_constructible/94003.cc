// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <optional>
#include <tuple>

template <bool B> struct abc {};

template <typename T>

struct future : public abc<std::is_trivially_constructible_v<std::tuple<T>>> {};

class mutation {
  mutation();
  friend class std::optional<mutation>;
};

using mutation_opt = std::optional<mutation>;

future<mutation_opt> foo();

template <typename Consumer> future<mutation_opt> consume_partitions() {
  return foo();
}

future<mutation_opt> bar() { return consume_partitions<int>(); }

future<mutation> zed();
future<mutation> apply_counter_update() { return zed(); }

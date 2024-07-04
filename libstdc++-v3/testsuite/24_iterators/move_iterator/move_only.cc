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

// { dg-do compile { target c++20 } }

#include <iterator>

struct move_only_iterator
{
  move_only_iterator() = default;
  move_only_iterator(move_only_iterator&&) = default;
  move_only_iterator& operator=(move_only_iterator&&) = default;

  move_only_iterator& operator++();
  move_only_iterator operator++(int);
  int& operator*() const;

  bool operator==(const move_only_iterator&) const;
};

template<> struct std::iterator_traits<move_only_iterator>
{
  using value_type = int;
  using difference_type = std::ptrdiff_t;
  using iterator_category = std::input_iterator_tag;
};

static_assert(std::input_iterator<move_only_iterator>);

template<typename T>
  concept has_member_base = requires (T t) {
    // LWG 3391 made the const& overload of move_iterator::base()
    // unconstrained and return a const reference.  So rather than checking
    // whether base() is valid (which is now trivially true in an unevaluated
    // context), the below now checks whether decay-copying base() is valid.
    [](auto){}(std::forward<T>(t).base());
  };

using move_only_move_iterator = std::move_iterator<move_only_iterator>;

static_assert( ! has_member_base<move_only_move_iterator&> );
static_assert( ! has_member_base<const move_only_move_iterator&> );
static_assert( has_member_base<move_only_move_iterator> );
static_assert( ! has_member_base<const move_only_move_iterator> );

void
test01()
{
  move_only_move_iterator m1 = std::make_move_iterator(move_only_iterator{});
  move_only_move_iterator m2;
  m2 = std::move(m1);
}

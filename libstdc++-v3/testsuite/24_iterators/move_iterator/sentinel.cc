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

#include <iterator>
#include <testsuite_hooks.h>

void
test01()
{
  using S = std::move_sentinel<const int*>;
  using M = std::move_iterator<int*>;

  static_assert( std::is_default_constructible_v<S> );
  static_assert( std::is_copy_constructible_v<S> );
  static_assert( std::is_copy_assignable_v<S> );
  static_assert( std::is_constructible_v<S, std::move_sentinel<int*>> );
  static_assert( std::is_assignable_v<S, std::move_sentinel<int*>> );

  constexpr S s;
  static_assert( s.base() == nullptr );

  constexpr M m;
  static_assert( m == s );
  static_assert( s == m );
  static_assert( !(m != s) );
  static_assert( !(s != m) );

  int i = 0;
  M m2(&i);
  VERIFY( m2 != s );
  VERIFY( s != m2 );
  VERIFY( !(m2 == s) );
  VERIFY( !(s == m2) );
}

void
test02()
{
  struct sentinel { int limit; };

  struct iterator
  {
    using iterator_category = std::input_iterator_tag;
    using value_type = int;
    using difference_type = std::ptrdiff_t;
    using reference = const int&;

    const int& operator*() const { return counter; }

    iterator& operator++() { ++counter; return *this; }

    iterator operator++(int) { auto i = *this; ++counter; return i; }

    bool operator==(sentinel s) const { return counter == s.limit; }

    int counter = 0;
  };

  static_assert( std::sentinel_for<sentinel, iterator> );

  iterator i;
  sentinel s{5};
  int count = 0;
  for (auto m = std::make_move_iterator(i); m != std::move_sentinel{s}; ++m)
    ++count;
  VERIFY( count == 5 );
}

int
main()
{
  test01();
  test02();
}

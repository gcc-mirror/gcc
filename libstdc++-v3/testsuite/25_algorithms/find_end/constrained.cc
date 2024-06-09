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

// { dg-do run { target c++20 } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

struct X { int i; };

void
test01()
{
  X x[] = { {10}, {11}, {2}, {6}, {8}, {10}, {11} };
  X y[] = { {10}, {11} };
  {

    test_container<X, forward_iterator_wrapper> c(x);
    auto res = ranges::find_end(c, y, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res)->i == 10 && std::get<1>(res) == ranges::end(c) );
    res = ranges::find_end(c, c, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res) == ranges::begin(c)
	    && std::get<1>(res) == ranges::end(c) );
  }

  {
    test_range<X, forward_iterator_wrapper> r(x);
    auto res = ranges::find_end(r, y, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res)->i == 10 && std::get<1>(res) == ranges::end(r) );
    res = ranges::find_end(r, r, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res) == ranges::begin(r)
	    && std::get<1>(res) == ranges::end(r) );
  }

  {
    test_range<X, bidirectional_iterator_wrapper> r(x);
    auto res = ranges::find_end(r, y, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res)->i == 10 && std::get<1>(res) == ranges::end(r) );
    res = ranges::find_end(r, r, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res) == ranges::begin(r)
	    && std::get<1>(res) == ranges::end(r) );
  }
}

void
test02()
{
  static constexpr X x[] = { {2}, {2}, {6}, {8}, {10}, {6}, {8}, {11} };
  static constexpr X y[] = { {6}, {8} };
  static constexpr int z[] = { 2, 8 };
  static constexpr int w[] = { 2 };

  static_assert(std::get<0>(ranges::find_end(x, y, {}, &X::i, &X::i)) == x+5);
  static_assert(std::get<1>(ranges::find_end(x, y, {}, &X::i, &X::i)) == x+7);

  static_assert(std::get<0>(ranges::find_end(x, z, {}, &X::i)) == x+8);
  static_assert(std::get<1>(ranges::find_end(x, z, {}, &X::i)) == x+8);

  static_assert(std::get<0>(ranges::find_end(x, w, {}, &X::i)) == x+1);
  static_assert(std::get<1>(ranges::find_end(x, w, {}, &X::i)) == x+2);

  static_assert(std::get<0>(ranges::find_end(x, x+6, w, w, {}, &X::i)) == x+6);
  static_assert(std::get<1>(ranges::find_end(x, x+6, w, w, {}, &X::i)) == x+6);

  static_assert(std::get<0>(ranges::find_end(x, x, w, w+1, {}, &X::i)) == x+0);
  static_assert(std::get<1>(ranges::find_end(x, x, w, w+1, {}, &X::i)) == x+0);
}

int
main()
{
  test01();
  test02();
}

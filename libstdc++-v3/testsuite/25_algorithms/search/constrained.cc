// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

struct X { int i; };

void
test01()
{
  X x[] = { {2}, {6}, {8}, {10}, {11} };
  X y[] = { {10}, {11} };
  {

    test_container<X, forward_iterator_wrapper> c(x);
    auto res = ranges::search(c, y, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res)->i == 10 && std::get<1>(res) == ranges::end(c) );
    res = ranges::search(c, c, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res) == ranges::begin(c)
	    && std::get<1>(res) == ranges::end(c) );
  }

  {
    test_range<X, forward_iterator_wrapper> r(x);
    auto res = ranges::search(r, y, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res)->i == 10 && std::get<1>(res) == ranges::end(r) );
    res = ranges::search(r, r, {}, &X::i, &X::i);
    VERIFY( std::get<0>(res) == ranges::begin(r)
	    && std::get<1>(res) == ranges::end(r) );
  }
}

void
test02()
{
  static constexpr X x[] = { {2}, {2}, {6}, {8}, {10}, {11} };
  static constexpr X y[] = { {6}, {8} };
  static constexpr int z[] = { 2, 8 };
  static constexpr int w[] = { 2 };

  static_assert(std::get<0>(ranges::search(x, y, {}, &X::i, &X::i)) == x+2);
  static_assert(std::get<1>(ranges::search(x, y, {}, &X::i, &X::i)) == x+4);

  static_assert(std::get<0>(ranges::search(x, z, {}, &X::i)) == x+6);
  static_assert(std::get<1>(ranges::search(x, z, {}, &X::i)) == x+6);

  static_assert(std::get<0>(ranges::search(x, w, {}, &X::i)) == x+0);
  static_assert(std::get<1>(ranges::search(x, w, {}, &X::i)) == x+1);

  static_assert(std::get<0>(ranges::search(x, x+6, w, w, {}, &X::i)) == x+0);
  static_assert(std::get<1>(ranges::search(x, x+6, w, w, {}, &X::i)) == x+0);

  static_assert(std::get<0>(ranges::search(x, x, w, w+1, {}, &X::i)) == x+0);
  static_assert(std::get<1>(ranges::search(x, x, w, w+1, {}, &X::i)) == x+0);
}

int
main()
{
  test01();
  test02();
}

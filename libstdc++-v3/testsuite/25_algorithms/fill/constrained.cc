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
// { dg-do run { target c++2a } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>
#include <list>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

struct X
{
  int i;
};

void
test01()
{
  const int c[6] = { 17, 17, 17, 17, 17, 17 };
    {
      X x[6];
      VERIFY( ranges::fill(x, X{17}) == x+6 );
      VERIFY( ranges::equal(x, c, {}, &X::i) );
    }

    {
      char x[6];
      VERIFY( ranges::fill(x, 17) == x+6 );
      VERIFY( ranges::equal(x, c) );
    }

    {
      X x[6];
      test_container<X, forward_iterator_wrapper> cx(x);
      VERIFY( ranges::fill(cx, X{17}) == cx.end() );
      VERIFY( ranges::equal(cx, c, {}, &X::i) );
    }

    {
      int x[6];
      test_range<int, output_iterator_wrapper> rx(x);
      VERIFY( ranges::fill(rx, 17) == rx.end() );
      VERIFY( ranges::equal(x, c) );
    }

    {
      std::list<int> list(6);
      ranges::fill(list, 17);
      VERIFY( ranges::equal(list, c) );
    }
}

constexpr bool
test02()
{
  bool ok = true;
  int x[5];
  ranges::fill(x, 17);
  for (auto v : x)
    ok &= v == 17;
  return ok;
}

int
main()
{
  test01();
  static_assert(test02());
}

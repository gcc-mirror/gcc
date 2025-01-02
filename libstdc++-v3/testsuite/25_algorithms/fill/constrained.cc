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

void
test03()
{
  // Bug libstdc++/117094 - ranges::fill misses std::move for output_iterator

  // Move-only output iterator
  struct Iterator
  {
    using difference_type = long;
    Iterator(int* p) : p(p) { }
    Iterator(Iterator&&) = default;
    Iterator& operator=(Iterator&&) = default;
    int& operator*() const { return *p; }
    Iterator& operator++() { ++p; return *this; }
    Iterator operator++(int) { return Iterator(p++ ); }
    int* p;

    struct Sentinel
    {
      const int* p;
      bool operator==(const Iterator& i) const { return p == i.p; }
      long operator-(const Iterator& i) const { return p - i.p; }
    };

    long operator-(Sentinel s) const { return p - s.p; }
  };
  static_assert(std::sized_sentinel_for<Iterator::Sentinel, Iterator>);
  int a[2];
  std::ranges::fill(Iterator(a), Iterator::Sentinel{a+2}, 999);
  VERIFY( a[0] == 999 );
  VERIFY( a[1] == 999 );
}

int
main()
{
  test01();
  static_assert(test02());
  test03();
}

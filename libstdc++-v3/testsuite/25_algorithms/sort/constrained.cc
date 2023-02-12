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
// { dg-require-cstdint "" }

#include <algorithm>
#include <random>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::random_access_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  for (unsigned size = 0; size < 50; ++size)
    {
      std::vector<int> vref(size);
      std::iota(vref.begin(), vref.end(), 0);
      std::vector<int> v1(vref), v2(vref);
      test_container<int, random_access_iterator_wrapper> c
	= {v1.data(), v1.data() + size};
      test_range<int, random_access_iterator_wrapper> r
	= {v2.data(), v2.data() + size};

      std::ranlux48_base g1(size), g2(size + 1);
      ranges::shuffle(c, g1);
      ranges::shuffle(ranges::begin(r), ranges::end(r), g2);

      VERIFY( ranges::sort(c) == c.end() );
      VERIFY( ranges::sort(r) == ranges::end(r) );

      VERIFY( ranges::equal(c, vref) );
      VERIFY( ranges::equal(r, vref) );
    }
}

struct X
{
  int i;
  constexpr X(int a) : i(a) { }
};

constexpr bool
test02()
{
  X x[] = {3,4,2,1,5};
  const X y[] = {4,3,2,1,5};

  auto res = ranges::sort(x, x+4, ranges::greater{}, &X::i);
  return (res == x+4
	  && ranges::equal(x, y, {}, &X::i, &X::i));
}

int
main()
{
  test01();
  static_assert(test02());
}

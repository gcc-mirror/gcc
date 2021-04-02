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
using __gnu_test::output_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
    {
      int x[] = {1,2,3,4,5,6,7,8,9,10,11};
      int y[5], z[6];
      test_container<int, forward_iterator_wrapper> cx(x);
      test_container<int, forward_iterator_wrapper> cy(y), cz(z);
      auto pred = [] (int a) { return a%2==0; };
      auto [in,out_true,out_false]
	= ranges::partition_copy(cx, cy.begin(), cz.begin(), pred);
      VERIFY( in.ptr == x+11 );
      VERIFY( out_true.ptr == y+5 );
      VERIFY( out_false.ptr == z+6 );
      VERIFY( ranges::all_of(cy, pred) );
      VERIFY( ranges::none_of(cz, pred) );
    }

    {
      int x[] = {1,2,3,4,5,6,7,8,9,10,11};
      int y[6], z[5];
      test_range<int, input_iterator_wrapper> cx(x);
      test_range<int, output_iterator_wrapper> cy(y), cz(z);
      auto pred = [] (int a) { return a%2==0; };
      auto proj = [] (int a) { return a+1; };
      auto [in,out_true,out_false]
	= ranges::partition_copy(cx, cy.begin(), cz.begin(), pred, proj);
      VERIFY( in.ptr == x+11 );
      VERIFY( out_true.ptr == y+6 );
      VERIFY( out_false.ptr == z+5 );
      VERIFY( ranges::none_of(y, pred) );
      VERIFY( ranges::all_of(z, pred) );
    }
}

constexpr bool
test02()
{
  int x[] = {1,2,3,4,5,6,7,8,9,10};
  auto range = ranges::partition(x, x+9, [] (int a) { return a < 100; });
  return (range.begin() == x+9 && range.end() == x+9);
}

int
main()
{
  test01();
  static_assert(test02());
}

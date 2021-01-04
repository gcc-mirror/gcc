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
  X xa[] = { {1}, {2}, {3}, {4}, {5}, {6} };
  X xb[] = { {1}, {2}, {3}, {3}, {5}, {6} };
  auto res = ranges::mismatch(xa, xa+6, xb, xb+6, {}, &X::i, &X::i);
  VERIFY( res.in1 == xa+3 && res.in2 == xb+3 );

  test_container<X, forward_iterator_wrapper> ca(xa);
  test_container<X, forward_iterator_wrapper> cb(xb);
  auto res2 = ranges::mismatch(ca, cb, {}, &X::i, &X::i);
  VERIFY( res2.in1->i == 4 && res2.in2->i == 3 );
  res2 = ranges::mismatch(ca, ca, {}, &X::i, &X::i);
  VERIFY( res2.in1 == ranges::end(ca) && res2.in2 == ranges::end(ca) );

  test_range<X, input_iterator_wrapper> ra(xa);
  test_range<X, input_iterator_wrapper> rb(xb);
  auto res3 = ranges::mismatch(ra, rb, {}, &X::i, &X::i);
  VERIFY( res3.in1->i == 4 && res3.in2->i == 3 );

  test_range<X, input_iterator_wrapper> ra2(xa);
  ra.bounds.first = xa;
  res3 = ranges::mismatch(ra, ra2, {}, &X::i, &X::i);
  VERIFY( res3.in1 == ranges::end(ra) && res3.in2 == ranges::end(ra2) );
}

struct Y { int i; int j; };

void
test02()
{
  static constexpr Y ya[] = { {1,2}, {2,4}, {3,6}, {1,6} };
  static constexpr Y yb[] = { {2,1}, {4,2}, {4,2}, {7,1} };
  static_assert(ranges::mismatch(ya, yb, {}, &Y::i, &Y::j).in1 == ya+2);
  static_assert(ranges::mismatch(ya, yb, {}, &Y::i, &Y::j).in2 == yb+2);
}

int
main()
{
  test01();
  test02();
}

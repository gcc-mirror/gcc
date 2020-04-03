// Copyright (C) 2020 Free Software Foundation, Inc.
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

namespace ranges = std::ranges;

template<typename T1, typename T2>
constexpr bool
operator==(const ranges::minmax_result<T1>& lhs,
	   const ranges::minmax_result<T2>& rhs)
{
  return (lhs.min == rhs.min
	  && rhs.max == rhs.max);
}


struct X
{
  int i, j;
};

using res_t = ranges::minmax_result<int>;

void
test01()
{
  VERIFY( ranges::minmax(1, 2) == res_t(1,2) );
  VERIFY( ranges::minmax(2, 1) == res_t(1,2) );
  VERIFY( ranges::minmax(1, 2, ranges::greater{}) == res_t(2,1) );
  VERIFY( ranges::minmax(1, 2, ranges::greater{}, std::negate<>{}) == res_t(1,2) );
  VERIFY( ranges::minmax(1, 2, {}, std::negate<>{}) == res_t(2,1) );
  VERIFY( ranges::minmax(X{1,2}, X{1,3}, {}, &X::i).min.j == 2 );
  VERIFY( ranges::minmax(X{1,2}, X{1,3}, {}, &X::i).max.j == 3 );
}

void
test02()
{
  int x[] = {1,2,3,4};
  do
    {
      test_range<int, input_iterator_wrapper> cx(x);
      VERIFY( ranges::minmax(cx) == res_t(1,4) );
      cx.bounds.first = x;
      VERIFY( ranges::minmax(cx, ranges::greater{}) == res_t(4,1) );
      cx.bounds.first = x;
      VERIFY( ranges::minmax(cx, {}, std::negate<>{}) == res_t(4,1));
      cx.bounds.first = x;
      VERIFY( ranges::minmax(cx, ranges::greater{}, std::negate<>{})
	      == res_t(1,4) );
    } while (ranges::next_permutation(x).found);

  constexpr X y[] = {{1,5},{1,2},{1,3}};
  static_assert(ranges::minmax(y, {}, &X::i).min.j == 5);
  static_assert(ranges::minmax(y, {}, &X::i).max.j == 3);
}

void
test03()
{
  VERIFY( ranges::minmax({2,3,1,4}) == res_t(1,4) );
  VERIFY( ranges::minmax({2,3,1,4}, ranges::greater{}) == res_t(4,1) );
  VERIFY( ranges::minmax({2,3,1,4}, {}, std::negate<>{}) == res_t(4,1) );
  VERIFY( ranges::minmax({2,3,1,4}, ranges::greater{}, std::negate<>{})
	  == res_t(1,4) );
}

int
main()
{
  test01();
  test02();
  test03();
}

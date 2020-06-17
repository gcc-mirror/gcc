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
#include <ranges>
#include <testsuite_iterators.h>
#include <testsuite_hooks.h>

using __gnu_test::bidirectional_iterator_wrapper;
using __gnu_test::forward_iterator_wrapper;
using __gnu_test::test_range;
using __gnu_test::test_sized_range;
using __gnu_test::test_sized_range_sized_sent;

namespace ranges = std::ranges;

template<typename Container>
void
test01()
{
  int x[] = {1,2,3,4,5};
  Container r{x};
  ranges::subrange sr = r;
  constexpr bool sized_range_p = ranges::sized_range<decltype(sr)>;
  constexpr bool bidirectional_p = ranges::bidirectional_range<decltype(sr)>;
  VERIFY( ranges::equal(sr, (int[]){1,2,3,4,5}) );
  if constexpr (sized_range_p)
    VERIFY( sr.size() == 5 );

  sr = sr.next();
  VERIFY( ranges::equal(sr, (int[]){2,3,4,5}) );
  if constexpr (sized_range_p)
    VERIFY( sr.size() == 4 );

  sr = std::move(sr).next(2);
  VERIFY( ranges::equal(sr, (int[]){4,5}) );
  if constexpr (sized_range_p)
    VERIFY( sr.size() == 2 );

  if constexpr (bidirectional_p)
    {
      sr = sr.prev(2);
      VERIFY( ranges::equal(sr, (int[]){2,3,4,5}) );
      if constexpr (sized_range_p)
	VERIFY( sr.size() == 4 );

      sr = sr.prev();
      VERIFY( ranges::equal(sr, (int[]){1,2,3,4,5}) );
      if constexpr (sized_range_p)
	VERIFY( sr.size() == 5 );
    }
  else
    sr = r;

  sr.advance(1);
  VERIFY( ranges::equal(sr, (int[]){2,3,4,5}) );
  if constexpr (sized_range_p)
    VERIFY( sr.size() == 4 );

  if constexpr (bidirectional_p)
    {
      sr.advance(-1);
      VERIFY( ranges::equal(sr, (int[]){1,2,3,4,5}) );
      if constexpr (sized_range_p)
	VERIFY( sr.size() == 5 );
    }
}

int
main()
{
  test01<test_sized_range_sized_sent<int, bidirectional_iterator_wrapper>>();
  test01<test_sized_range<int, bidirectional_iterator_wrapper>>();
  test01<test_range<int, bidirectional_iterator_wrapper>>();

  test01<test_sized_range_sized_sent<int, forward_iterator_wrapper>>();
  test01<test_sized_range<int, forward_iterator_wrapper>>();
  test01<test_range<int, forward_iterator_wrapper>>();
}

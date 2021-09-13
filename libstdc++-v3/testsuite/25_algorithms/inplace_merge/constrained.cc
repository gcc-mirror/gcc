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
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_container;
using __gnu_test::test_range;
using __gnu_test::bidirectional_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1,2,3,4,5};
  for (int i = 0; i <= 5; i++)
    for (int j = 0; j <= 5; j++)
      {
	std::vector<int> v(x, x+i);
	v.insert(v.end(), x, x+j);
	ranges::sort(v);

	test_range<int, bidirectional_iterator_wrapper> rz(v.data(), v.data()+i+j);
	auto result = ranges::inplace_merge(rz, ranges::next(ranges::begin(rz), i));
	VERIFY( result == rz.end() );

	VERIFY( ranges::is_sorted(rz) );
      }
}

void
test02()
{
  struct X { int i, j; };
  X x[] = { {1, 1}, {3, 4}, {5, 5}, {2, 2}, {2, 3} };
  auto comp = ranges::greater{};
  auto proj = [] (X a) { return -a.i; };
  ranges::inplace_merge(x, x+3, x+5, comp, proj);
  VERIFY( ranges::is_sorted(x, {}, &X::i) );
  VERIFY( ranges::is_sorted(x, {}, &X::j) );
}


int
main()
{
  test01();
  test02();
}

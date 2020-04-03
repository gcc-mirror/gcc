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
using __gnu_test::forward_iterator_wrapper;

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1, 2, 3, 4, 5};
  char y[] = {1, 2, 3, 5};
  long z[] = {1, 2, 3, 4, 5, 6};

    {
      test_range<int, input_iterator_wrapper> rx(x);
      test_range<char, input_iterator_wrapper> ry(y);
      test_range<long, input_iterator_wrapper> rz(z);

      VERIFY( ranges::lexicographical_compare(rx, ry) );
      rx.bounds.first = x;
      ry.bounds.first = y;
      VERIFY( !ranges::lexicographical_compare(ry, rx) );
    }

  test_range<int, forward_iterator_wrapper> rx(x);
  test_range<char, forward_iterator_wrapper> ry(y);
  test_range<long, forward_iterator_wrapper> rz(z);

  VERIFY( ranges::lexicographical_compare(rx, rz) );
  VERIFY( !ranges::lexicographical_compare(rz, rx) );

  VERIFY( !ranges::lexicographical_compare(rx, rx) );
  VERIFY( ranges::lexicographical_compare(rx, rx, {}, std::negate<>{}) );
  VERIFY( ranges::lexicographical_compare(rx, rx, std::greater{},
					  {}, std::negate<>{}) );

  VERIFY( !ranges::lexicographical_compare(rx, ry, {},
					   std::negate<>{},
					   std::negate<>{}) );
  VERIFY( ranges::lexicographical_compare(ry, rx, {},
					  std::negate<>{},
					  std::negate<>{}) );

  VERIFY( ranges::lexicographical_compare(rx, rz, ranges::greater{}) );
  VERIFY( !ranges::lexicographical_compare(rz, rx, ranges::greater{}) );

  VERIFY( ranges::lexicographical_compare(rx, ry, ranges::greater{},
					  std::negate<>{},
					  std::negate<>{}) );
  VERIFY( !ranges::lexicographical_compare(ry, rx, ranges::greater{},
					   std::negate<>{},
					   std::negate<>{}) );
}

void
test02()
{
  int x[] = {1, 2, 3, 4, 5};
  int y[] = {1, 2, 3, 5};
  int z[] = {1, 2, 3, 4, 5, 6};

  VERIFY( ranges::lexicographical_compare(x, y) );
  VERIFY( !ranges::lexicographical_compare(y, x) );

  VERIFY( ranges::lexicographical_compare(x, z) );
  VERIFY( !ranges::lexicographical_compare(z, x) );

  VERIFY( !ranges::lexicographical_compare(x, x) );

  VERIFY( !ranges::lexicographical_compare(x, y, {},
					   std::negate<>{},
					   std::negate<>{}) );
  VERIFY( ranges::lexicographical_compare(y, x, {},
					  std::negate<>{},
					  std::negate<>{}) );

  VERIFY( ranges::lexicographical_compare(x, z, ranges::greater{}) );
  VERIFY( !ranges::lexicographical_compare(z, x, ranges::greater{}) );

  VERIFY( ranges::lexicographical_compare(x, y, ranges::greater{},
					  std::negate<>{},
					  std::negate<>{}) );
  VERIFY( !ranges::lexicographical_compare(y, x, ranges::greater{},
					   std::negate<>{},
					   std::negate<>{}) );
}

void
test03()
{
  int x[] = {1, 2, 3, 4, 5};
  int y[] = {1, 2, 5, 3};
  int z[] = {1, 2, 3, 5};

  do
    {
      VERIFY( ranges::lexicographical_compare(x, y) );
      VERIFY( !ranges::lexicographical_compare(x, y, ranges::greater{}) );
      VERIFY( !ranges::lexicographical_compare(y, x) );
      VERIFY( ranges::lexicographical_compare(y, x, ranges::greater{}) );

      test_container<int, forward_iterator_wrapper> cy(y);
      VERIFY( ranges::lexicographical_compare(x, cy) );
      VERIFY( !ranges::lexicographical_compare(x, cy, ranges::greater{}) );
      VERIFY( !ranges::lexicographical_compare(cy, x) );
      VERIFY( ranges::lexicographical_compare(cy, x, ranges::greater{}) );

      test_container<int, forward_iterator_wrapper> cz(z);
      VERIFY( ranges::lexicographical_compare(cz.begin(), cz.end(),
					      cy.begin(), cy.end()) );
      VERIFY( !ranges::lexicographical_compare(cy.begin(), cy.end(),
					       cz.begin(), cz.end()) );

      std::vector<int> vx(x, x+5), vy(y, y+5);
      VERIFY( ranges::lexicographical_compare(vx, vy) );
      VERIFY( !ranges::lexicographical_compare(vx, vy, ranges::greater{}) );
      VERIFY( !ranges::lexicographical_compare(vy, vx) );
      VERIFY( ranges::lexicographical_compare(vy, vx, ranges::greater{}) );
    } while (ranges::next_permutation(y).found);
}

constexpr bool
test04()
{
  int x[] = {1};
  int y[] = {1};
  return (ranges::lexicographical_compare((int[]){1,2,3,5},
					  (int[]){1,2,4})
	  && !ranges::lexicographical_compare(x, x, y, y));
}

int
main()
{
  test01();
  test02();
  test03();
  static_assert(test04());
}

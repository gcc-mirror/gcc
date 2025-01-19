// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

using __gnu_test::test_output_range;

namespace ranges = std::ranges;

template<typename Out, auto value>
void
test01()
{
    {
      Out x[5];
      ranges::fill_n(x, 5, value);
      VERIFY( ranges::count(x, static_cast<Out>(value)) == ranges::size(x) );
    }

    {
      Out x[5];
      test_output_range<Out> rx(x);
      ranges::fill_n(x, 5, value);
      VERIFY( ranges::count(x, static_cast<Out>(value)) == ranges::size(x) );
    }
}

int
main()
{
  test01<char, 'a'>();
  test01<char, 100>();
  test01<char, 150>();
  test01<char, 300>();
  test01<char, 1000>();
  test01<char, -10000L>();

  test01<signed char, 'a'>();
  test01<signed char, 100>();
  test01<signed char, 150>();
  test01<signed char, 300>();

  test01<unsigned char, 'a'>();
  test01<unsigned char, 100>();
  test01<unsigned char, 150>();
  test01<unsigned char, 300>();

  test01<int, 'a'>();
  test01<int, u8'a'>();
  test01<int, (signed char)'a'>();
  test01<int, (unsigned char)'a'>();

  test01<volatile int, 'a'>();
  test01<volatile int, 'a'>();
  test01<volatile int, 500>();
  test01<volatile char, 500>();
}

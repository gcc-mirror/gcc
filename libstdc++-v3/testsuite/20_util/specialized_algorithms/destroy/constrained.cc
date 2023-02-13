// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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
#include <cstring>
#include <deque>
#include <list>
#include <memory>
#include <span>
#include <string>
#include <vector>

#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_range;
using __gnu_test::input_iterator_wrapper_nocopy;

namespace ranges = std::ranges;

struct X
{
  X()
  { ++count; }

  ~X()
  { --count; }

  static inline int count = 0;
};

void
test01()
{
  for (int k = 0; k < 3; k++)
    {
      constexpr int size = 1024;
      auto buffer = std::unique_ptr<char[]>(new char[sizeof(X)*size]);
      std::span<X> rx((X *)buffer.get(), size);

      ranges::uninitialized_default_construct(rx);
      VERIFY( X::count == size );

      auto i = rx.begin();
      if (k == 0)
	i = ranges::destroy(rx);
      else if (k == 1)
	i = ranges::destroy(rx.begin(), rx.end());
      else if (k == 2)
	i = ranges::destroy_n(rx.begin(), size);
      else
	__builtin_abort();

      VERIFY( i == rx.end() );
      VERIFY( X::count == 0 );
    }
}

void
test02()
{
  // LWG 3355
    {
      int x[3] = {0};
      test_range<int, input_iterator_wrapper_nocopy> rx(x);
      ranges::destroy(rx);
      ranges::destroy_n(rx.begin(), 3);
    }
}

int
main()
{
  test01();
}

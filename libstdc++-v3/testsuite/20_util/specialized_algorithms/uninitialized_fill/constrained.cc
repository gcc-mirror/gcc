// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
#include <utility>

#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

using __gnu_test::test_forward_range;

namespace ranges = std::ranges;

template<typename T>
void
test01(const T& value)
{
  static_assert(std::equality_comparable<T>);

  for (int k = 0; k < 6; k++)
    {
      constexpr int size = 1024;
      auto buffer = std::unique_ptr<char[]>(new char[sizeof(T)*size]);
      std::span<T> rx((T *)buffer.get(), size);

      auto i = rx.begin();
      if (k == 0)
	i = ranges::uninitialized_fill(rx.begin(), rx.end(), value);
      else if (k == 1)
	i = ranges::uninitialized_fill(rx, value);
      else if (k == 2)
	i = ranges::uninitialized_fill_n(rx.begin(), 1024, value);
      else if (k == 3)
	i = ranges::uninitialized_fill(rx.begin(), rx.end(), value);
      else if (k == 4)
	i = ranges::uninitialized_fill(std::as_const(rx), value);
      else if (k == 5)
	i = ranges::uninitialized_fill_n(rx.begin(), 1024, value);
      else
	__builtin_abort();

      VERIFY( i == rx.end() );
      VERIFY( ranges::find_if(rx, [&value](const T& v) { return value != v; }) == i );

      ranges::destroy(rx);
    }
}

struct X
{
  static constexpr int limit = 67;
  static inline int construct_count = 0;
  static inline int destruct_count = 0;

  struct exception {};

  bool live = false;

  X(int)
  {
    if (construct_count >= limit)
      throw exception{};
    construct_count++;
    live = true;
  }

  ~X()
  {
    VERIFY( live );
    live = false;
    destruct_count++;
  }
};

template<bool test_sized>
void
test02()
{
  constexpr int size = 100;
  auto buffer = std::unique_ptr<char[]>(new char[sizeof(X)*size]);
  test_forward_range<X> rx((X *)buffer.get(), (X *)buffer.get() + size);
  int value = 5;
  try
    {
      X::construct_count = 0;
      X::destruct_count = 0;
      if constexpr (test_sized)
	ranges::uninitialized_fill_n(rx.begin(), size, value);
      else
	ranges::uninitialized_fill(rx, value);
      VERIFY( false && "exception not thrown" );
    }
  catch (const X::exception&)
    {
      VERIFY( X::construct_count == X::limit );
      VERIFY( X::destruct_count == X::limit );
    }
}

int
main()
{
  test01<char>(5);
  test01<int>(3);
  test01<long long>(17);
  test01<float>(2.18);
  test01<double>(3.98);
  test01<std::vector<char>>({'a', 'b', 'c', 'd'});
  test01<std::string>("hello");
  test01<std::deque<double>>({1.1,2.1,3.1});
  test01<std::list<std::vector<std::deque<double>>>>({{{3.4},{1}},{{7.9}}});

  test02<false>();
  test02<true>();
}

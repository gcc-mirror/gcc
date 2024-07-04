// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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
test01()
{
  static_assert(std::default_initializable<T>);
  static_assert(std::equality_comparable<T>);

  for (int k = 0; k < 6; k++)
    {
      constexpr int size = 1024;
      auto buffer = std::unique_ptr<char[]>(new char[sizeof(T)*size]);
      std::span<T> rx((T *)buffer.get(), size);

      T t{};

      auto i = rx.begin();
      if (k == 0)
	i = ranges::uninitialized_value_construct(rx.begin(), rx.end());
      else if (k == 1)
	i = ranges::uninitialized_value_construct(rx);
      else if (k == 2)
	i = ranges::uninitialized_value_construct_n(rx.begin(), 1024);
      else if (k == 3)
	i = ranges::uninitialized_value_construct(rx.begin(), rx.end());
      else if (k == 4)
	i = ranges::uninitialized_value_construct(std::as_const(rx));
      else if (k == 5)
	i = ranges::uninitialized_value_construct_n(rx.begin(), 1024);
      else
	__builtin_abort();

      VERIFY( i == rx.end() );
      VERIFY( ranges::find_if(rx, [&t](const T& v) { return t != v; }) == i );

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

  X()
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
  try
    {
      X::construct_count = 0;
      X::destruct_count = 0;
      if constexpr (test_sized)
	ranges::uninitialized_value_construct_n(rx.begin(), size);
      else
	ranges::uninitialized_value_construct(rx);
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
  test01<char>();
  test01<int>();
  test01<long long>();
  test01<float>();
  test01<double>();
  test01<std::vector<char>>();
  test01<std::string>();
  test01<std::deque<double>>();
  test01<std::list<std::vector<std::deque<double>>>>();
  test01<std::unique_ptr<std::string>>();

  test02<false>();
  test02<true>();
}

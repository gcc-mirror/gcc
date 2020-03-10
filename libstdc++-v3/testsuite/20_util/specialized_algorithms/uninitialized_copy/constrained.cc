// Copyright (C) 2020 Free Software Foundation, Inc.
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

using __gnu_test::test_input_range;
using __gnu_test::test_forward_range;
using __gnu_test::test_range;
using __gnu_test::test_sized_range_sized_sent;
using __gnu_test::input_iterator_wrapper_nocopy;

namespace ranges = std::ranges;

template<typename T>
void
test01(const std::vector<T> &ix)
{
  static_assert(std::copy_constructible<T>);
  static_assert(std::equality_comparable<T>);

  for (int k = 0; k < 7; k++)
    {
      int size = ix.size();
      auto buffer = std::unique_ptr<char[]>(new char[sizeof(T)*size]);
      std::span<T> rx((T *)buffer.get(), size);

      ranges::uninitialized_copy_result res = {ix.begin(), rx.begin()};
      if (k == 0)
	res = ranges::uninitialized_copy(ix.begin(), ix.end(),
					 rx.begin(), rx.end());
      else if (k == 1)
	res = ranges::uninitialized_copy(ix, rx);
      else if (k == 2)
	res = ranges::uninitialized_copy_n(ix.begin(), size,
					   rx.begin(), rx.end());
      else if (k == 3)
	res = ranges::uninitialized_copy(ix.begin(), ix.end(),
					 rx.begin(), rx.end());
      else if (k == 4)
	res = ranges::uninitialized_copy(ix, std::as_const(rx));
      else if (k == 5)
	res = ranges::uninitialized_copy_n(ix.begin(), size,
					   rx.begin(), rx.end());
      else if (k == 6)
	res = ranges::uninitialized_copy_n(ix.begin(), size/2,
					   rx.begin(), rx.end());
      else if (k == 7)
	res = ranges::uninitialized_copy_n(ix.begin(), size,
					   rx.begin(), rx.begin()+size/2);
      else
	__builtin_abort();

      if (k == 6 || k == 7)
	{
	  VERIFY( ranges::distance(ix.begin(), res.in) == size/2 );
	  VERIFY( ranges::distance(rx.begin(), res.out) == size/2 );
	  VERIFY( ranges::equal(ix.begin(), ix.begin()+size/2,
				rx.begin(), rx.begin()+size/2) );
	  ranges::destroy(rx.begin(), rx.begin()+size/2);
	}
      else
	{
	  VERIFY( res.in == ix.end() );
	  VERIFY( res.out == rx.end() );
	  VERIFY( ranges::equal(ix, rx) );
	  ranges::destroy(rx);
	}
    }
}

struct X
{
  static constexpr int limit = 67;
  static inline int copy_construct_count = 0;
  static inline int destruct_count = 0;

  struct exception {};

  bool live = false;

  X()
  { live = true; }

  X& operator=(const X&) = delete;

  X(const X&)
  {
    live = true;
    if (copy_construct_count >= limit)
      throw exception{};
    copy_construct_count++;
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
  X x[size];
  // FIXME: Should be test_input_range?
  test_forward_range<X> ix(x);

  auto buffer = std::unique_ptr<char[]>(new char[sizeof(X)*size]);
  test_forward_range<X> rx((X *)buffer.get(), (X *)buffer.get() + size);
  try
    {
      X::copy_construct_count = 0;
      X::destruct_count = 0;
      if constexpr (test_sized)
	ranges::uninitialized_copy_n(ix.begin(), size, rx.begin(), rx.end());
      else
	ranges::uninitialized_copy(ix, rx);
      VERIFY( false && "exception not thrown" );
    }
  catch (const X::exception&)
    {
      VERIFY( X::copy_construct_count == X::limit );
      VERIFY( X::destruct_count == X::limit );
    }
}

void
test03()
{
  // LWG 3355
    {
      int x[3] = {0};
      int y[3];
      test_sized_range_sized_sent<int, input_iterator_wrapper_nocopy> rx(x);
      ranges::uninitialized_copy(rx, y);
      ranges::uninitialized_copy_n(rx.begin(), 3, y, y+3);
    }

    {
      int x[3] = {0};
      int y[3];
      test_range<int, input_iterator_wrapper_nocopy> rx(x);
      test_forward_range<int> ry(y);
      ranges::uninitialized_copy(rx, y);
      ranges::uninitialized_copy_n(rx.begin(), 3, ry.begin(), ry.end());
    }
}

int
main()
{
  test01<char>({1,2,3,4,5});
  test01<int>({1,2,3,4,5});
  test01<long long>({1,2,3,4,5});
  test01<float>({1.1,2.1,3.1,4.1});
  test01<double>({1.1,2.1,3.1,4.1});
  test01<std::vector<char>>({{'a','b'}, {'c','d'}, {'e','f'}});
  test01<std::string>({"the", "quick", "brown", "fox"});

  test02<false>();
  test02<true>();
}

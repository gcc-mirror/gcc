// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

using __gnu_test::test_input_range;
using __gnu_test::test_forward_range;
using __gnu_test::test_range;
using __gnu_test::test_sized_range_sized_sent;
using __gnu_test::input_iterator_wrapper_nocopy;

namespace ranges = std::ranges;

template<typename T>
void
test01(std::vector<T> ix)
{
  static_assert(std::move_constructible<T>);
  static_assert(std::equality_comparable<T>);

  const auto saved_ix = ix;

  for (int k = 0; k < 7; k++)
    {
      ix = saved_ix;

      int size = ix.size();
      auto buffer = std::unique_ptr<char[]>(new char[sizeof(T)*size]);
      std::span<T> rx((T *)buffer.get(), size);

      ranges::uninitialized_move_result res = {ix.begin(), rx.begin()};
      if (k == 0)
	res = ranges::uninitialized_move(ix.begin(), ix.end(),
					 rx.begin(), rx.end());
      else if (k == 1)
	res = ranges::uninitialized_move(ix, rx);
      else if (k == 2)
	res = ranges::uninitialized_move_n(ix.begin(), size,
					   rx.begin(), rx.end());
      else if (k == 3)
	res = ranges::uninitialized_move(ix.begin(), ix.end(),
					 rx.begin(), rx.end());
      else if (k == 4)
	res = ranges::uninitialized_move(ix, std::as_const(rx));
      else if (k == 5)
	res = ranges::uninitialized_move_n(ix.begin(), size,
					   rx.begin(), rx.end());
      else if (k == 6)
	res = ranges::uninitialized_move_n(ix.begin(), size/2,
					   rx.begin(), rx.end());
      else if (k == 7)
	res = ranges::uninitialized_move_n(ix.begin(), size,
					   rx.begin(), rx.begin()+size/2);
      else
	__builtin_abort();

      if (k == 6 || k == 7)
	{
	  VERIFY( ranges::distance(ix.begin(), res.in) == size/2 );
	  VERIFY( ranges::distance(rx.begin(), res.out) == size/2 );
	  VERIFY( ranges::equal(saved_ix.begin(), saved_ix.begin()+size/2,
				rx.begin(), rx.begin()+size/2) );
	  ranges::destroy(rx.begin(), rx.begin()+size/2);
	}
      else
	{
	  VERIFY( res.in == ix.end() );
	  VERIFY( res.out == rx.end() );
	  VERIFY( ranges::equal(saved_ix, rx) );
	  ranges::destroy(rx);
	}
    }
}

struct X
{
  static constexpr int limit = 67;
  static inline int move_construct_count = 0;
  static inline int destruct_count = 0;

  struct exception {};

  bool live = false;
  bool moved_from = false;

  X()
  { live = true; moved_from = false; }

  X& operator=(const X&) = delete;
  X(const X&) = delete;

  X&& operator=(X&&) = delete;

  X(X&& other)
  {
    VERIFY( !other.moved_from );
    other.moved_from = true;
    live = true;
    if (move_construct_count >= limit)
      throw exception{};
    move_construct_count++;
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
      X::move_construct_count = 0;
      X::destruct_count = 0;
      if constexpr (test_sized)
	ranges::uninitialized_move_n(ix.begin(), size, rx.begin(), rx.end());
      else
	ranges::uninitialized_move(ix, rx);
      VERIFY( false && "exception not thrown" );
    }
  catch (const X::exception&)
    {
      VERIFY( X::move_construct_count == X::limit );
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
      ranges::uninitialized_move(rx, y);
      ranges::uninitialized_move_n(rx.begin(), 3, y, y+3);
    }

    {
      int x[3] = {0};
      int y[3];
      test_range<int, input_iterator_wrapper_nocopy> rx(x);
      test_forward_range<int> ry(y);
      ranges::uninitialized_move(rx, y);
      ranges::uninitialized_move_n(rx.begin(), 3, ry.begin(), ry.end());
    }
}

void
test_pr101587()
{
  short in[1]{};
  __gnu_test::test_contiguous_range r(in); // difference_type is integer-like
  long out[1];
  std::span<long> o(out); // difference_type is ptrdiff_t
  ranges::uninitialized_move(r, o);
  ranges::uninitialized_move_n(ranges::begin(r), 0, o.begin(), o.end());

  struct Iter
  {
    using value_type = long;
    using difference_type = std::ranges::__detail::__max_diff_type;

    long& operator*() const { return *p; }

    Iter& operator++() { ++p; return *this; }
    Iter operator++(int) { return Iter{p++}; }

    difference_type operator-(Iter i) const { return p - i.p; }
    bool operator==(const Iter&) const = default;

    long* p = nullptr;
  };
  static_assert(std::sized_sentinel_for<Iter, Iter>);

  std::ranges::subrange<Iter> rmax(Iter{out+0}, Iter{out+1});
  // Check with integer-like class type for output range:
  std::ranges::uninitialized_move(in, rmax);
  std::ranges::uninitialized_move_n(in+0, 1, rmax.begin(), rmax.end());

  int to[1];
  // And for input range:
  std::ranges::uninitialized_copy(rmax, to);
  std::ranges::uninitialized_copy_n(rmax.begin(), 1, to+0, to+1);
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

  test_pr101587();
}

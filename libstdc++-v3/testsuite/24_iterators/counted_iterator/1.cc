// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#include <iterator>
#include <testsuite_hooks.h>

void
test01()
{
  using I = std::counted_iterator<int*>;
  static_assert( std::is_default_constructible_v<I> );
  static_assert( std::is_copy_constructible_v<I> );
  static_assert( std::is_copy_assignable_v<I> );
  static_assert( ! std::is_constructible_v<I, int*> );
  static_assert( std::is_constructible_v<I, int*, std::ptrdiff_t> );

  using J = std::counted_iterator<const int*>;
  static_assert( std::is_constructible_v<J, const I&> );
  static_assert( std::is_convertible_v<const I&, J> );
}

void
test02()
{
  int in[3] = { 1, 2, 3 };
  std::counted_iterator<const int*> in_iter(std::begin(in), std::ssize(in));
  VERIFY( in_iter.base() == in );
  VERIFY( (std::default_sentinel - in_iter) == 3 );
  VERIFY( (in_iter - std::default_sentinel) == -3 );

  int out[4] = { };
  std::counted_iterator<int*> out_iter(std::begin(out), std::ssize(out));
  VERIFY( out_iter.base() == out );
  VERIFY( (std::default_sentinel - out_iter) == 4 );
  VERIFY( (out_iter - std::default_sentinel) == -4 );

  while (in_iter != std::default_sentinel && out_iter != std::default_sentinel)
    *out_iter++ = *in_iter++;

  VERIFY(in_iter == std::default_sentinel);
  VERIFY(out_iter != std::default_sentinel);
  VERIFY( out[0] == 1 );
  VERIFY( out[1] == 2 );
  VERIFY( out[2] == 3 );
  VERIFY( out[3] == 0 );

  auto out2 = out_iter;
  out2 += 1;
  VERIFY( out2 == std::default_sentinel );
  VERIFY( (out2 <=> out_iter) == std::strong_ordering::greater );
  out2 -= 3;
  VERIFY( (out_iter - out2) == 2 );
  VERIFY( (out2 <=> out_iter) == std::strong_ordering::less );
}

void
test03()
{
  struct X
  {
    X(int i) : i(i) { }
    X(X&& x) : i(x.i) { x.i = -1; }
    X& operator=(X&& x) { i = x.i; x.i = 0; return *this; }
    int i;
  };

  X arr[] = { 1, 2 };
  std::counted_iterator<X*> i(arr, 2), j(arr + 1, 1);
  std::ranges::iter_swap(i, j);
  VERIFY( arr[0].i == 2 );
  VERIFY( arr[1].i == 1 );

  X x = std::ranges::iter_move(i);
  VERIFY( arr[0].i == -1 );
  VERIFY( x.i == 2 );
}

int
main()
{
  test01();
  test02();
  test03();
}

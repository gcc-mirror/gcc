// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <iterator>
#include <testsuite_hooks.h>

void
test01()
{
  using I = std::common_iterator<int*, const int*>;
  static_assert( std::is_default_constructible_v<I> );
  static_assert( std::is_copy_constructible_v<I> );
  static_assert( std::is_copy_assignable_v<I> );
  static_assert( std::is_constructible_v<I, int*> );
  static_assert( std::is_constructible_v<I, const int*> );

  struct sentinel { operator int*() const { return nullptr; } };
  using K = std::common_iterator<int*, sentinel>;
  static_assert( std::is_constructible_v<I, const K&> );
  static_assert( std::is_assignable_v<I, const K&> );

  struct sentinel2
  {
    const int* p;
    sentinel2(const int* p = 0) : p(p) { }
    bool operator==(const int* p) const { return p == this->p; }
  };

  using J = std::common_iterator<const int*, sentinel2>;
  static_assert( std::is_constructible_v<J, const I&> );
  static_assert( std::is_convertible_v<const I&, J> );
}

void
test02()
{
  struct sentinel { int limit; };

  struct iterator
  {
    using iterator_category = std::input_iterator_tag;
    using value_type = int;
    using difference_type = std::ptrdiff_t;
    using reference = const int&;

    const int& operator*() const { return counter; }

    iterator& operator++() { ++counter; return *this; }

    iterator operator++(int) { auto i = *this; ++counter; return i; }

    bool operator==(sentinel s) const { return counter == s.limit; }

    int counter = 0;
  };

  static_assert( std::sentinel_for<sentinel, iterator> );

  int out[5] = { };
  std::common_iterator<int*, const int*> obegin = std::begin(out);
  std::common_iterator<int*, const int*> oend = std::cend(out);

  iterator i;
  sentinel s{5};
  std::common_iterator<iterator, sentinel> begin = i, end = s;
  while (begin != end)
    *obegin++ = *begin++;

  VERIFY(obegin == oend);
  for (int& i : out)
    VERIFY( i == (&i - out) );
}

void
test03()
{
  int arr[2] = { 1, 2 };
  std::common_iterator<int*, const int*> i = std::ranges::begin(arr);
  std::common_iterator<int*, const int*> end = std::ranges::cend(arr);
  VERIFY( i != end );
  VERIFY( (end - i) == 2 );
  VERIFY( (i - end) == -2 );
  auto j = i;
  VERIFY( j == i );
  VERIFY( (j - i) == 0 );
  j = end;
  VERIFY( j != i );
  VERIFY( j == end );
  j = std::ranges::next(i);
  VERIFY( j != i );
  VERIFY( j != end );
  VERIFY( (end - j) == 1 );
  VERIFY( (j - i) == 1 );
  VERIFY( (i - j) == -1 );
  ++j;
  VERIFY( j == end );
  VERIFY( (end - j) == 0 );
  j = i;
  VERIFY( j == i );
  VERIFY( (j - end) == -2 );
  VERIFY( (j - i) == 0 );

  try
  {
    struct S { operator const int*() const { throw 1; } };
    i = std::common_iterator<int*, S>(S{});
    VERIFY( false );
  }
  catch (int)
  {
  }
}

void
test04()
{
  struct X
  {
    X(int i) : i(i) { }
    X(X&& x) : i(x.i) { x.i = -1; }
    X& operator=(X&& x) { i = x.i; x.i = 0; return *this; }
    int i;
  };

  X arr[] = { 1, 2 };
  std::common_iterator<X*, const X*> i(arr), j(arr+1);
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
  test04();
}

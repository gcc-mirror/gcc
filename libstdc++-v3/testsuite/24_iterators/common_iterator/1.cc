// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
  static_assert( std::is_move_constructible_v<I> );
  static_assert( std::is_copy_assignable_v<I> );
  static_assert( std::is_move_assignable_v<I> );
  static_assert( std::is_constructible_v<I, int*> );
  static_assert( std::is_constructible_v<I, const int*> );

  static_assert( std::is_nothrow_copy_constructible_v<I> ); // GCC extension
  static_assert( std::is_nothrow_move_constructible_v<I> ); // GCC extension
  static_assert( std::is_nothrow_copy_assignable_v<I> ); // GCC extension
  static_assert( std::is_nothrow_move_assignable_v<I> ); // GCC extension

  struct sentinel { operator int*() const noexcept { return nullptr; } };
  using K = std::common_iterator<int*, sentinel>;
  static_assert( std::is_constructible_v<I, const K&> );
  static_assert( std::is_assignable_v<I, const K&> );

  static_assert( std::is_nothrow_assignable_v<I&, const K&> ); // GCC extension

  struct sentinel_throwing { operator int*() const { return nullptr; } };
  using K_throwing = std::common_iterator<int*, sentinel_throwing>;
  // Conversion is noexcept(false)
  static_assert( ! std::is_nothrow_assignable_v<I&, const K_throwing&> );


  struct sentinel2
  {
    const int* p;
    sentinel2(const int* p = 0) : p(p) { }
    bool operator==(const int* p) const { return p == this->p; }
  };

  using J = std::common_iterator<const int*, sentinel2>;
  static_assert( std::is_constructible_v<J, const I&> );
  static_assert( std::is_convertible_v<const I&, J> );

  static_assert( std::is_constructible_v<J, I> );
  static_assert( std::is_convertible_v<I, J> );

  // Constructor is noexcept(false)
  static_assert( ! std::is_nothrow_constructible_v<J, I> );
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

constexpr bool
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

  if (std::is_constant_evaluated())
    return true;

  try
  {
    struct S { operator const int*() const { throw 1; } };
    i = std::common_iterator<int*, S>(S{});
    VERIFY( false );
  }
  catch (int)
  {
  }

  return true;
}

static_assert( test03() );

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

constexpr bool
test_pr103992()
{
  using C1 = std::common_iterator<std::reverse_iterator<int*>,
				  std::unreachable_sentinel_t>;
  using C2 = std::common_iterator<std::reverse_iterator<const int*>,
				  std::unreachable_sentinel_t>;
  C1 c1;
  C2 c2 = c1;
  C1 c3 = c1;

  return true;
}

static_assert( test_pr103992() );

int
main()
{
  test01();
  test02();
  test03();
  test04();
}

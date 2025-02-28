// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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
#include <testsuite_iterators.h>

static_assert(__gnu_test::is_customization_point_object(std::ranges::iter_move));

struct X
{
  int value;

  constexpr X(int i) : value(i) { }

  X(const X&) = default;
  X& operator=(const X&) = default;

  constexpr X(X&& x)
  : value(x.value)
  {
    x.value = -2;
  }

  constexpr X& operator=(X&& x)
  {
    value = x.value;
    x.value = -1;
    return *this;
  }
};

constexpr bool
test_X(int i, int j)
{
  X x1{i}, x2{j};
  (void) std::ranges::iter_move(&x1); // no-op
  x1 = std::ranges::iter_move(&x2);
  return x1.value == j &&  x2.value == -1;
}

static_assert( test_X(1, 2) );

void
test01()
{
  VERIFY( test_X(3, 4) );
}

template<typename T>
using rval_ref = std::iter_rvalue_reference_t<T>;

static_assert(std::same_as<rval_ref<int*>, int&&>);
static_assert(std::same_as<rval_ref<const int*>, const int&&>);
static_assert(std::same_as<rval_ref<std::move_iterator<int*>>, int&&>);

template<typename T>
concept iter_movable = requires { std::ranges::iter_move(std::declval<T>()); };

struct Iter
{
  friend int& iter_move(Iter&) { static int i = 1; return i; }
  friend long iter_move(Iter&&) { return 2; }
  const short& operator*() const & { static short s = 3; return s; }
  friend float operator*(const Iter&&) { return 4.0f; }
};

void
test_adl()
{
  Iter it;
  const Iter& cit = it;

  VERIFY( std::ranges::iter_move(it) == 1 );
  VERIFY( std::ranges::iter_move(std::move(it)) == 2 );
  VERIFY( std::ranges::iter_move(cit) == 3 );
  VERIFY( std::ranges::iter_move(std::move(cit)) == 4.0f );

  // The return type should be unchanged for ADL iter_move:
  static_assert(std::same_as<decltype(std::ranges::iter_move(it)), int&>);
  static_assert(std::same_as<decltype(std::ranges::iter_move(std::move(it))),
			     long>);
  // When ADL iter_move is not used, return type should be an rvalue:
  static_assert(std::same_as<decltype(std::ranges::iter_move(cit)),
			     const short&&>);
  static_assert(std::same_as<decltype(std::ranges::iter_move(std::move(cit))),
			     float>);

  // std::iter_rvalue_reference_t always considers the argument as lvalue.
  static_assert(std::same_as<rval_ref<Iter>, int&>);
  static_assert(std::same_as<rval_ref<Iter&>, int&>);
  static_assert(std::same_as<rval_ref<const Iter>, const short&&>);
  static_assert(std::same_as<rval_ref<const Iter&>, const short&&>);
}

void
test_pr106612()
{
  // Bug 106612 ranges::iter_move does not consider iterator's value categories

  struct I
  {
    int i{};
    int& operator*() & { return i; }
    int operator*() const & { return i; }
    void operator*() && = delete;
  };

  static_assert( iter_movable<I&> );
  static_assert( iter_movable<I const&> );
  static_assert( ! iter_movable<I> );
  static_assert( std::same_as<std::iter_rvalue_reference_t<I>, int&&> );
  static_assert( std::same_as<std::iter_rvalue_reference_t<const I>, int> );

  struct I2
  {
    int i{};
    int& operator*() & { return i; }
    int operator*() const & { return i; }
    void operator*() &&;
  };

  static_assert( iter_movable<I2&> );
  static_assert( iter_movable<I2 const&> );
  static_assert( iter_movable<I2> );
  static_assert( std::is_void_v<decltype(std::ranges::iter_move(I2{}))> );
  static_assert( std::same_as<std::iter_rvalue_reference_t<I2>, int&&> );
  static_assert( std::same_as<std::iter_rvalue_reference_t<I2 const>, int> );

  enum E { e };
  enum F { f };

  struct I3
  {
    E operator*() const & { return e; }
    F operator*() && { return f; }
  };

  static_assert( iter_movable<I3&> );
  static_assert( iter_movable<I3> );
  static_assert( std::same_as<decltype(std::ranges::iter_move(I3{})), F> );
}

int
main()
{
  test01();
  test_adl();
}

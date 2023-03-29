// { dg-do run { target c++14 } }

// Copyright (C) 2015-2023 Free Software Foundation, Inc.
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

// 8.2.1 Class template shared_ptr [memory.smartptr.shared]

#include <experimental/memory>
#include <testsuite_hooks.h>

int destroyed = 0;

struct A : std::experimental::enable_shared_from_this<A>
{
  ~A() { ++destroyed; }
};

struct B : A { };

// 8.2.1.1 shared_ptr constructors [memory.smartptr.shared.const]

// Construction from unique_ptr<A[]>

template<typename From, typename To>
constexpr bool constructible()
{
  using std::experimental::shared_ptr;
  using std::experimental::is_constructible_v;
  using std::unique_ptr;
  return is_constructible_v<shared_ptr<To>, unique_ptr<From>>;
}

static_assert(  constructible< A,    A    >(), "A -> A compatible" );
static_assert( !constructible< A,    A[]  >(), "A -> A[] not compatible" );
static_assert( !constructible< A,    A[1] >(), "A -> A[1] not compatible" );
static_assert( !constructible< A[],  A    >(), "A[] -> A not compatible" );
static_assert(  constructible< A[],  A[]  >(), "A[] -> A[] compatible" );
static_assert( !constructible< A[],  A[1] >(), "A[] -> A[1] not compatible" );

static_assert(  constructible< B,    A    >(), "B -> A compatible" );
static_assert( !constructible< B,    A[]  >(), "B -> A[] not compatible" );
static_assert( !constructible< B,    A[1] >(), "B -> A[1] not compatible" );
static_assert( !constructible< B[],  A    >(), "B[] -> A not compatible" );
static_assert( !constructible< B[],  A[]  >(), "B[] -> A[] not compatible" );
static_assert( !constructible< B[],  A[1] >(), "B[2] -> A[1] not compatible" );

void
test01()
{
  std::unique_ptr<A> up(new A);
  std::experimental::shared_ptr<A> sp(std::move(up));
  VERIFY( up.get() == 0 );
  VERIFY( sp.get() != 0 );
  VERIFY( sp.use_count() == 1 );

  VERIFY( sp->shared_from_this() != nullptr );

  sp.reset();
  VERIFY( destroyed == 1 );
  destroyed = 0;
}

void
test02()
{
  std::unique_ptr<A[]> up(new A[5]);
  std::experimental::shared_ptr<A[]> sp(std::move(up));
  VERIFY( up.get() == 0 );
  VERIFY( sp.get() != 0 );
  VERIFY( sp.use_count() == 1 );

  bool caught = false;
  try
  {
    sp[0].shared_from_this(); // should not be set for arrays
  }
  catch (const std::bad_weak_ptr&)
  {
    caught = true;
  }
  VERIFY( caught );

  sp.reset();
  VERIFY( destroyed == 5 );
}

int
main()
{
  test01();
  test02();
}

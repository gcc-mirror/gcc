// { dg-do run { target c++14 } }

// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

struct A { };
struct B : A { };

// 8.2.1.1 shared_ptr constructors [memory.smartptr.shared.const]

template<typename From, typename To>
constexpr bool constructible()
{
  using std::experimental::shared_ptr;
  using std::experimental::weak_ptr;
  using std::experimental::is_constructible_v;
  return is_constructible_v<shared_ptr<To>, weak_ptr<From>>
    && is_constructible_v<shared_ptr<To>, const weak_ptr<From>&>;
}

static_assert(  constructible< A,    A    >(), "A -> A compatible" );
static_assert( !constructible< A,    A[]  >(), "A -> A[] not compatible" );
static_assert( !constructible< A,    A[1] >(), "A -> A[1] not compatible" );
static_assert( !constructible< A[],  A    >(), "A[] -> A not compatible" );
static_assert(  constructible< A[],  A[]  >(), "A[] -> A[] compatible" );
static_assert( !constructible< A[],  A[1] >(), "A[] -> A[1] not compatible" );
static_assert( !constructible< A[1], A    >(), "A[1] -> A not compatible" );
static_assert(  constructible< A[1], A[]  >(), "A[1] -> A[] compatible" );
static_assert(  constructible< A[1], A[1] >(), "A[1] -> A[1] compatible" );
static_assert( !constructible< A[2], A[1] >(), "A[2] -> A[1] not compatible" );

static_assert(  constructible< B,    A    >(), "B -> A compatible" );
static_assert( !constructible< B,    A[]  >(), "B -> A[] not compatible" );
static_assert( !constructible< B,    A[1] >(), "B -> A[1] not compatible" );
static_assert( !constructible< B[],  A    >(), "B[] -> A not compatible" );
static_assert( !constructible< B[],  A[]  >(), "B[] -> A[] not compatible" );
static_assert( !constructible< B[],  A[1] >(), "B[] -> A[1] not compatible" );
static_assert( !constructible< B[1], A    >(), "B[] -> A not compatible" );
static_assert( !constructible< B[1], A[]  >(), "B[] -> A[] not compatible" );
static_assert( !constructible< B[1], A[1] >(), "B[] -> A[1] not compatible" );
static_assert( !constructible< B[2], A[1] >(), "B[2] -> A[1] not compatible" );



// Construction from weak_ptr
void
test01()
{
  A * a = new A[5];
  std::experimental::shared_ptr<A[5]> a1(a);
  std::experimental::weak_ptr<A[5]> wa(a1);
  std::experimental::shared_ptr<A[5]> a2(wa);
  std::experimental::shared_ptr<A[5]> a3 = wa.lock();
  VERIFY( a2.get() == a );
  VERIFY( a3.get() == a );
  VERIFY( a2.use_count() == wa.use_count() );
  VERIFY( a3.use_count() == wa.use_count() );
}

int
main()
{
  test01();
}

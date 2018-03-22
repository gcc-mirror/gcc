// { dg-do run { target c++11 } }

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

// 20.7.2.2 Class template shared_ptr [util.smartptr.shared]

#include <memory>
#include <testsuite_hooks.h>

struct A { };

int destroyed = 0;
struct B : A { ~B() { ++destroyed; } };

// 20.7.2.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Construction from unique_ptr

template<typename From, typename To>
constexpr bool constructible()
{
  using namespace std;
  return is_constructible<shared_ptr<To>, unique_ptr<From>>::value
    && is_constructible<shared_ptr<const To>, unique_ptr<From>>::value
    && is_constructible<shared_ptr<const To>, unique_ptr<const From>>::value;
}

static_assert(  constructible< A,   A    >(), "A -> A compatible" );
static_assert(  constructible< B,   A    >(), "B -> A compatible" );
static_assert(  constructible< int, int  >(), "int -> int compatible" );
static_assert( !constructible< int, long >(), "int -> long not compatible" );

void
test01()
{
  std::unique_ptr<A> up(new A);
  std::shared_ptr<A> sp(std::move(up));
  VERIFY( up.get() == 0 );
  VERIFY( sp.get() != 0 );
  VERIFY( sp.use_count() == 1 );
}

void
test02()
{
  std::unique_ptr<B> b(new B);
  std::shared_ptr<A> a(std::move(b));
  VERIFY( b.get() == 0 );
  VERIFY( a.get() != 0 );
  VERIFY( a.use_count() == 1 );
  a.reset();
  VERIFY( destroyed == 1 );
}

int
main()
{
  test01();
  test02();
  return 0;
}

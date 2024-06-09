// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

// C++17 20.11.2.2.1 shared_ptr constructors [util.smartptr.shared.const]

template<typename To, typename From>
constexpr bool check()
{
  using std::shared_ptr;
  using std::is_constructible;
  return !is_constructible<shared_ptr<To>, shared_ptr<From>>::value
    && !is_constructible<shared_ptr<To>, shared_ptr<From>&>::value;
}

static_assert( check<int, int[]>() );
static_assert( check<int, int[2]>() );
static_assert( check<int[2], void>() );
static_assert( check<int[2], int>() );
static_assert( check<int[2], int[]>() );
static_assert( check<int[], void>() );
static_assert( check<int[], int>() );

int count = 0;

struct A {
  A() { ++count; }
  ~A() { --count; }
};

struct B : A { };

static_assert( check<A, B[2]>() );
static_assert( check<A, B[]>() );
static_assert( check<A[2], B>() );
static_assert( check<A[2], B[2]>() );
static_assert( check<A[2], B[]>() );
static_assert( check<A[], B>() );
static_assert( check<A[], B[2]>() );
static_assert( check<A[], B[]>() );

void
test01()
{
  std::shared_ptr<A[2]> p;
  VERIFY( p.get() == nullptr );
  VERIFY( p.use_count() == 0 );
  p.reset();
  VERIFY( count == 0 );
}

void
test02()
{
  std::shared_ptr<A[]> p;
  VERIFY( p.get() == nullptr );
  VERIFY( p.use_count() == 0 );
  p.reset();
  VERIFY( count == 0 );
}

void
test03()
{
  std::shared_ptr<A[2]> p(nullptr);
  VERIFY( p.get() == nullptr );
  VERIFY( p.use_count() == 0 );
  p.reset();
  VERIFY( count == 0 );
}

void
test04()
{
  std::shared_ptr<A[]> p(nullptr);
  VERIFY( p.get() == nullptr );
  VERIFY( p.use_count() == 0 );
  p.reset();
  VERIFY( count == 0 );
}

// Construction from pointer

void
test05()
{
  A * const a = nullptr;
  std::shared_ptr<A[2]> p(a);
  VERIFY( p.get() == nullptr );
  VERIFY( p.use_count() == 1 );
  p.reset();
  VERIFY( count == 0 );
}

void
test06()
{
  A * const a = nullptr;
  std::shared_ptr<A[]> p(a);
  VERIFY( p.get() == nullptr );
  VERIFY( p.use_count() == 1 );
  p.reset();
  VERIFY( count == 0 );
}

void
test07()
{
  A * const a = new A[2];
  std::shared_ptr<A[2]> p(a);
  VERIFY( p.get() == a );
  VERIFY( p.use_count() == 1 );
  p.reset();
  VERIFY( count == 0 );
}

void
test08()
{
  A * const a = new A[2];
  std::shared_ptr<A[]> p(a);
  VERIFY( p.get() == a );
  VERIFY( p.use_count() == 1 );
  p.reset();
  VERIFY( count == 0 );
}

// Converting constructor

void
test09()
{
  A * const a = new A[2];
  std::shared_ptr<A[2]> p(a);
  std::shared_ptr<const A[2]> p2(p);
  VERIFY( p2.get() == a );
  VERIFY( p.use_count() == 2 );
  VERIFY( p2.use_count() == 2 );
  p.reset();
  VERIFY( count != 0 );
  p2.reset();
  VERIFY( count == 0 );
}

void
test10()
{
  A * const a = new A[2];
  std::shared_ptr<A[]> p(a);
  std::shared_ptr<const A[]> p2(p);
  VERIFY( p2.get() == a );
  VERIFY( p.use_count() == 2 );
  VERIFY( p2.use_count() == 2 );
  p.reset();
  VERIFY( count != 0 );
  p2.reset();
  VERIFY( count == 0 );
}

void
test11()
{
  A * const a = new A[2];
  std::shared_ptr<A[2]> p(a);
  std::shared_ptr<const A[]> p2(p);
  VERIFY( p2.get() == a );
  VERIFY( p.use_count() == 2 );
  VERIFY( p2.use_count() == 2 );
  p.reset();
  VERIFY( count != 0 );
  p2.reset();
  VERIFY( count == 0 );
}

// Copy construction

void
test12()
{
  A * const a = new A[2];
  std::shared_ptr<A[2]> p(a);
  std::shared_ptr<A[2]> p2(p);
  VERIFY( p2.get() == a );
  VERIFY( p.use_count() == 2 );
  VERIFY( p2.use_count() == 2 );
  p.reset();
  VERIFY( count != 0 );
  p2.reset();
  VERIFY( count == 0 );
}

void
test13()
{
  A * const a = new A[2];
  std::shared_ptr<A[2]> p(a);
  std::shared_ptr<A[]> p2(p);
  VERIFY( p2.get() == a );
  VERIFY( p.use_count() == 2 );
  VERIFY( p2.use_count() == 2 );
  p.reset();
  VERIFY( count != 0 );
  p2.reset();
  VERIFY( count == 0 );
}

// Move construction

void
test14()
{
  A * const a = new A[2];
  std::shared_ptr<A[2]> p(a);
  std::shared_ptr<A[2]> p2(std::move(p));
  VERIFY( p.get() == nullptr );
  VERIFY( p2.get() == a );
  VERIFY( p.use_count() == 0 );
  VERIFY( p2.use_count() == 1 );
  p2.reset();
  VERIFY( count == 0 );
}

void
test15()
{
  A * const a = new A[2];
  std::shared_ptr<A[2]> p(a);
  std::shared_ptr<A[]> p2(std::move(p));
  VERIFY( p.get() == nullptr );
  VERIFY( p2.get() == a );
  VERIFY( p.use_count() == 0 );
  VERIFY( p2.use_count() == 1 );
  p2.reset();
  VERIFY( count == 0 );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  test05();
  test06();
  test07();
  test08();
  test09();
  test10();
  test11();
  test12();
  test13();
  test14();
  test15();
}

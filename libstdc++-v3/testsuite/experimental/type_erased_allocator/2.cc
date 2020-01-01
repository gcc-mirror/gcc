// { dg-do run { target c++14 } }
// { dg-require-thread-fence "" }

// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

#include <experimental/memory_resource>
#include <experimental/utility>
#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using std::experimental::pmr::polymorphic_allocator;
using std::experimental::pmr::memory_resource;
using std::experimental::pmr::new_delete_resource;
using std::experimental::pmr::get_default_resource;
using std::allocator_arg_t;

enum CtorType { Default, Copy, Move, Other, Tuple, Piecewise_Default, Piecewise_Copy};

// type that takes a memory_resource before other ctor args
struct A
{
  using allocator_type = std::experimental::erased_type;

  CtorType type;
  memory_resource* alloc = nullptr;

  A() : type(Default) { }
  A(allocator_arg_t, memory_resource* a) : type(Default), alloc(a) { }
  A(const A&) : type(Copy) { }
  A(allocator_arg_t, memory_resource* a, const A&) : type(Copy), alloc(a) { }
  A(A&&) : type (Move) { }
  A(allocator_arg_t, memory_resource* a, A&&) : type (Move), alloc(a) { }
  A(int) : type(Other) { }
  A(allocator_arg_t, memory_resource* a, int) : type(Other), alloc(a) { }
};

// type that takes a memory_resource after other ctor args
struct B
{
  using allocator_type = std::experimental::erased_type;

  CtorType type;
  memory_resource* alloc = nullptr;

  B() : type(Default) { }
  B(memory_resource* a) : type(Default), alloc(a) { }
  B(const B&) : type(Copy) { }
  B(const B&, memory_resource* a) : type(Copy), alloc(a) { }
  B(B&&) : type (Move) { }
  B(B&&, memory_resource* a) : type(Move), alloc(a) { }
  B(int) : type(Other) { }
  B(int, memory_resource* a) : type(Other), alloc(a) { }
};

// type that takes no memory_resource
struct C
{
  CtorType type;
  C() : type(Default) { }
  C(const C&) : type(Copy) { }
  C(C&&) : type(Move) { }
  C(int) : type(Other) { }
};

// test construct for type that
// uses memory_resource* as allocator
template<typename A>
void test_uses_alloc() {
  polymorphic_allocator<A> pa;
  A* p = pa.allocate(1);
  A a;

  pa.construct(p);
  VERIFY(p->alloc == get_default_resource());
  VERIFY(p->type == Default);
  pa.destroy(p);

  pa.construct(p, a);
  VERIFY(p->type == Copy);
  pa.destroy(p);

  pa.construct(p, A());
  VERIFY(p->type == Move);
  pa.destroy(p);

  pa.construct(p, 1);
  VERIFY(p->type == Other);
  pa.destroy(p);

  pa.deallocate(p, 1);
}

// test construct for type that not using allocator
template <typename C>
void test_non_alloc() {
  polymorphic_allocator<C> pa;
  C* p = pa.allocate(1);
  C b;

  pa.construct(p);
  VERIFY(p->type == Default);
  pa.destroy(p);

  pa.construct(p, b);
  VERIFY(p->type == Copy);
  pa.destroy(p);

  pa.construct(p, C());
  VERIFY(p->type == Move);
  pa.destroy(p);

  pa.construct(p, 1);
  VERIFY(p->type == Other);
  pa.destroy(p);

  pa.deallocate(p, 1);
}

// test piecewise_construct
template <typename A, typename B>
void test_pair() {
  polymorphic_allocator<std::pair<A, B>> pa;
  std::pair<A, B>* p = pa.allocate(1);
  std::tuple<> t;

  // construct(pair<T1, T2>* p, piecewise_construct_t, tuple<...>, tuple<...>)
  pa.construct(p, std::piecewise_construct, t, t);
  VERIFY(p->first.type == Default);
  VERIFY(p->second.type == Default);
  pa.destroy(p);

  // construct(pair<T1, T2>* __p)
  pa.construct(p);
  VERIFY(p->first.type == Default);
  VERIFY(p->second.type == Default);
  pa.destroy(p);

  // construct(pair<T1, T2>* p, U&& x, V&& y)
  A a; B b;
  pa.construct(p, a, b);
  VERIFY(p->first.type == Copy);
  VERIFY(p->second.type == Copy);
  pa.destroy(p);

  pa.construct(p, A(), B());
  VERIFY(p->first.type == Move);
  VERIFY(p->second.type == Move);
  auto pp = *p;
  pa.destroy(p);

  // construct(pair<T1, T2>* p, const pair<U, V>& x)
  pa.construct(p, pp);
  VERIFY(p->first.type == Copy);
  VERIFY(p->second.type == Copy);
  pa.destroy(p);

  // construct(pair<T1, T2>* p, pair<U, V>&& x)
  pa.construct(p, std::move(pp));
  VERIFY(p->first.type == Move);
  VERIFY(p->second.type == Move);
  pa.destroy(p);
  pa.deallocate(p, 1);
}

void test01() {
  test_uses_alloc<A>();
  test_uses_alloc<B>();
  test_non_alloc<C>();
}

void test02() {
  test_pair<A, A>();
  test_pair<A, B>();
  test_pair<A, C>();
  test_pair<B, B>();
  test_pair<B, A>();
  test_pair<B, C>();
  test_pair<C, C>();
}


int main() {
  test01();
  test02();
}

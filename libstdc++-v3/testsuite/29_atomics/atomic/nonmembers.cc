// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }
// { dg-require-atomic-builtins "" }

#include <atomic>

void
test01()
{
  struct X { };
  struct Y { };
  // Primary template
  volatile std::atomic<X> v;
  std::atomic<Y> a;
  const std::memory_order mo = std::memory_order_seq_cst;
  X x;
  Y y;
  auto r1 = atomic_is_lock_free(&v);
  static_assert( std::is_same<decltype(r1), bool>::value, "" );
  auto r2 = atomic_is_lock_free(&a);
  static_assert( std::is_same<decltype(r2), bool>::value, "" );
  atomic_init(&v, x);
  atomic_init(&a, y);
  atomic_store(&v, x);
  atomic_store(&a, y);
  atomic_store_explicit(&v, x, mo);
  atomic_store_explicit(&a, y, mo);
  auto r3 = atomic_load(&v);
  static_assert( std::is_same<decltype(r3), X>::value, "" );
  auto r4 = atomic_load(&a);
  static_assert( std::is_same<decltype(r4), Y>::value, "" );
  auto r5 = atomic_load_explicit(&v, mo);
  static_assert( std::is_same<decltype(r5), X>::value, "" );
  auto r6 = atomic_load_explicit(&a, mo);
  static_assert( std::is_same<decltype(r6), Y>::value, "" );
  auto r7 = atomic_exchange(&v, x);
  static_assert( std::is_same<decltype(r7), X>::value, "" );
  auto r8 = atomic_exchange(&a, y);
  static_assert( std::is_same<decltype(r8), Y>::value, "" );
  auto r9 = atomic_exchange_explicit(&v, x, mo);
  static_assert( std::is_same<decltype(r9), X>::value, "" );
  auto r10 = atomic_exchange_explicit(&a, y, mo);
  static_assert( std::is_same<decltype(r10), Y>::value, "" );
  auto r11 = atomic_compare_exchange_weak(&v, &x, x);
  static_assert( std::is_same<decltype(r11), bool>::value, "" );
  auto r12 = atomic_compare_exchange_weak(&a, &y, y);
  static_assert( std::is_same<decltype(r12), bool>::value, "" );
  auto r13 = atomic_compare_exchange_strong(&v, &x, x);
  static_assert( std::is_same<decltype(r13), bool>::value, "" );
  auto r14 = atomic_compare_exchange_strong(&a, &y, y);
  static_assert( std::is_same<decltype(r14), bool>::value, "" );
  auto r15 = atomic_compare_exchange_weak_explicit(&v, &x, x, mo, mo);
  static_assert( std::is_same<decltype(r15), bool>::value, "" );
  auto r16 = atomic_compare_exchange_weak_explicit(&a, &y, y, mo, mo);
  static_assert( std::is_same<decltype(r16), bool>::value, "" );
  auto r17 = atomic_compare_exchange_strong_explicit(&v, &x, x, mo, mo);
  static_assert( std::is_same<decltype(r17), bool>::value, "" );
  auto r18 = atomic_compare_exchange_strong_explicit(&a, &y, y, mo, mo);
  static_assert( std::is_same<decltype(r18), bool>::value, "" );
}

void
test02()
{
  // Specialization for bool
  volatile std::atomic<bool> v;
  std::atomic<bool> a;
  const std::memory_order mo = std::memory_order_seq_cst;
  bool b = false;
  auto r1 = atomic_is_lock_free(&v);
  static_assert( std::is_same<decltype(r1), bool>::value, "" );
  auto r2 = atomic_is_lock_free(&a);
  static_assert( std::is_same<decltype(r2), bool>::value, "" );
  atomic_init(&v, b);
  atomic_init(&a, b);
  atomic_store(&v, b);
  atomic_store(&a, b);
  atomic_store_explicit(&v, b, mo);
  atomic_store_explicit(&a, b, mo);
  auto r3 = atomic_load(&v);
  static_assert( std::is_same<decltype(r3), bool>::value, "" );
  auto r4 = atomic_load(&a);
  static_assert( std::is_same<decltype(r4), bool>::value, "" );
  auto r5 = atomic_load_explicit(&v, mo);
  static_assert( std::is_same<decltype(r5), bool>::value, "" );
  auto r6 = atomic_load_explicit(&a, mo);
  static_assert( std::is_same<decltype(r6), bool>::value, "" );
  auto r7 = atomic_exchange(&v, b);
  static_assert( std::is_same<decltype(r7), bool>::value, "" );
  auto r8 = atomic_exchange(&a, b);
  static_assert( std::is_same<decltype(r8), bool>::value, "" );
  auto r9 = atomic_exchange_explicit(&v, b, mo);
  static_assert( std::is_same<decltype(r9), bool>::value, "" );
  auto r10 = atomic_exchange_explicit(&a, b, mo);
  static_assert( std::is_same<decltype(r10), bool>::value, "" );
  auto r11 = atomic_compare_exchange_weak(&v, &b, b);
  static_assert( std::is_same<decltype(r11), bool>::value, "" );
  auto r12 = atomic_compare_exchange_weak(&a, &b, b);
  static_assert( std::is_same<decltype(r12), bool>::value, "" );
  auto r13 = atomic_compare_exchange_strong(&v, &b, b);
  static_assert( std::is_same<decltype(r13), bool>::value, "" );
  auto r14 = atomic_compare_exchange_strong(&a, &b, b);
  static_assert( std::is_same<decltype(r14), bool>::value, "" );
  auto r15 = atomic_compare_exchange_weak_explicit(&v, &b, b, mo, mo);
  static_assert( std::is_same<decltype(r15), bool>::value, "" );
  auto r16 = atomic_compare_exchange_weak_explicit(&a, &b, b, mo, mo);
  static_assert( std::is_same<decltype(r16), bool>::value, "" );
  auto r17 = atomic_compare_exchange_strong_explicit(&v, &b, b, mo, mo);
  static_assert( std::is_same<decltype(r17), bool>::value, "" );
  auto r18 = atomic_compare_exchange_strong_explicit(&a, &b, b, mo, mo);
  static_assert( std::is_same<decltype(r18), bool>::value, "" );
}

void
test03()
{
  // Partial specialization for pointers
  volatile std::atomic<int*> v;
  std::atomic<long*> a;
  const std::memory_order mo = std::memory_order_seq_cst;
  int* i = nullptr;
  long* l = nullptr;
  auto r1 = atomic_is_lock_free(&v);
  static_assert( std::is_same<decltype(r1), bool>::value, "" );
  auto r2 = atomic_is_lock_free(&a);
  static_assert( std::is_same<decltype(r2), bool>::value, "" );
  atomic_init(&v, i);
  atomic_init(&a, l);
  atomic_store(&v, i);
  atomic_store(&a, l);
  atomic_store_explicit(&v, i, mo);
  atomic_store_explicit(&a, l, mo);
  auto r3 = atomic_load(&v);
  static_assert( std::is_same<decltype(r3), int*>::value, "" );
  auto r4 = atomic_load(&a);
  static_assert( std::is_same<decltype(r4), long*>::value, "" );
  auto r5 = atomic_load_explicit(&v, mo);
  static_assert( std::is_same<decltype(r5), int*>::value, "" );
  auto r6 = atomic_load_explicit(&a, mo);
  static_assert( std::is_same<decltype(r6), long*>::value, "" );
  auto r7 = atomic_exchange(&v, i);
  static_assert( std::is_same<decltype(r7), int*>::value, "" );
  auto r8 = atomic_exchange(&a, l);
  static_assert( std::is_same<decltype(r8), long*>::value, "" );
  auto r9 = atomic_exchange_explicit(&v, i, mo);
  static_assert( std::is_same<decltype(r9), int*>::value, "" );
  auto r10 = atomic_exchange_explicit(&a, l, mo);
  static_assert( std::is_same<decltype(r10), long*>::value, "" );
  auto r11 = atomic_compare_exchange_weak(&v, &i, i);
  static_assert( std::is_same<decltype(r11), bool>::value, "" );
  auto r12 = atomic_compare_exchange_weak(&a, &l, l);
  static_assert( std::is_same<decltype(r12), bool>::value, "" );
  auto r13 = atomic_compare_exchange_strong(&v, &i, i);
  static_assert( std::is_same<decltype(r13), bool>::value, "" );
  auto r14 = atomic_compare_exchange_strong(&a, &l, l);
  static_assert( std::is_same<decltype(r14), bool>::value, "" );
  auto r15 = atomic_compare_exchange_weak_explicit(&v, &i, i, mo, mo);
  static_assert( std::is_same<decltype(r15), bool>::value, "" );
  auto r16 = atomic_compare_exchange_weak_explicit(&a, &l, l, mo, mo);
  static_assert( std::is_same<decltype(r16), bool>::value, "" );
  auto r17 = atomic_compare_exchange_strong_explicit(&v, &i, i, mo, mo);
  static_assert( std::is_same<decltype(r17), bool>::value, "" );
  auto r18 = atomic_compare_exchange_strong_explicit(&a, &l, l, mo, mo);
  static_assert( std::is_same<decltype(r18), bool>::value, "" );

  auto r19 = atomic_fetch_add(&v, 1);
  static_assert( std::is_same<decltype(r19), int*>::value, "" );
  auto r20 = atomic_fetch_add(&a, 1);
  static_assert( std::is_same<decltype(r20), long*>::value, "" );
  auto r21 = atomic_fetch_add_explicit(&v, 1, mo);
  static_assert( std::is_same<decltype(r21), int*>::value, "" );
  auto r22 = atomic_fetch_add_explicit(&a, 1, mo);
  static_assert( std::is_same<decltype(r22), long*>::value, "" );
  auto r23 = atomic_fetch_sub(&v, 1);
  static_assert( std::is_same<decltype(r23), int*>::value, "" );
  auto r24 = atomic_fetch_sub(&a, 1);
  static_assert( std::is_same<decltype(r24), long*>::value, "" );
  auto r25 = atomic_fetch_sub_explicit(&v, 1, mo);
  static_assert( std::is_same<decltype(r25), int*>::value, "" );
  auto r26 = atomic_fetch_sub_explicit(&a, 1, mo);
  static_assert( std::is_same<decltype(r26), long*>::value, "" );
}

void
test04()
{
  struct base { };
  struct derived : base { };
  // Partial specialization for pointers
  volatile std::atomic<base*> v;
  std::atomic<base*> a;
  const std::memory_order mo = std::memory_order_seq_cst;
  // Repeat tests with arguments of type different to value_type.
  derived* const p = nullptr;
  base* b = nullptr;
  atomic_init(&v, p);
  atomic_init(&a, p);
  atomic_store(&v, p);
  atomic_store(&a, p);
  atomic_store_explicit(&v, p, mo);
  atomic_store_explicit(&a, p, mo);
  atomic_exchange(&v, p);
  atomic_exchange(&a, p);
  atomic_exchange_explicit(&v, p, mo);
  atomic_exchange_explicit(&a, p, mo);
  atomic_compare_exchange_weak(&v, &b, p);
  atomic_compare_exchange_weak(&a, &b, p);
  atomic_compare_exchange_strong(&v, &b, p);
  atomic_compare_exchange_strong(&a, &b, p);
  atomic_compare_exchange_weak_explicit(&v, &b, p, mo, mo);
  atomic_compare_exchange_weak_explicit(&a, &b, p, mo, mo);
  atomic_compare_exchange_strong_explicit(&v, &b, p, mo, mo);
  atomic_compare_exchange_strong_explicit(&a, &b, p, mo, mo);
}

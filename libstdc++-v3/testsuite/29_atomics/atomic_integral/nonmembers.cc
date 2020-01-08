// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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
  volatile std::atomic<int> v;
  std::atomic<long> a;
  const std::memory_order mo = std::memory_order_seq_cst;
  int i = 0;
  long l = 0;
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
  static_assert( std::is_same<decltype(r3), int>::value, "" );
  auto r4 = atomic_load(&a);
  static_assert( std::is_same<decltype(r4), long>::value, "" );
  auto r5 = atomic_load_explicit(&v, mo);
  static_assert( std::is_same<decltype(r5), int>::value, "" );
  auto r6 = atomic_load_explicit(&a, mo);
  static_assert( std::is_same<decltype(r6), long>::value, "" );
  auto r7 = atomic_exchange(&v, i);
  static_assert( std::is_same<decltype(r7), int>::value, "" );
  auto r8 = atomic_exchange(&a, l);
  static_assert( std::is_same<decltype(r8), long>::value, "" );
  auto r9 = atomic_exchange_explicit(&v, i, mo);
  static_assert( std::is_same<decltype(r9), int>::value, "" );
  auto r10 = atomic_exchange_explicit(&a, l, mo);
  static_assert( std::is_same<decltype(r10), long>::value, "" );
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

  auto r19 = atomic_fetch_add(&v, i);
  static_assert( std::is_same<decltype(r19), int>::value, "" );
  auto r20 = atomic_fetch_add(&a, l);
  static_assert( std::is_same<decltype(r20), long>::value, "" );
  auto r21 = atomic_fetch_add_explicit(&v, i, mo);
  static_assert( std::is_same<decltype(r21), int>::value, "" );
  auto r22 = atomic_fetch_add_explicit(&a, l, mo);
  static_assert( std::is_same<decltype(r22), long>::value, "" );
  auto r23 = atomic_fetch_sub(&v, i);
  static_assert( std::is_same<decltype(r23), int>::value, "" );
  auto r24 = atomic_fetch_sub(&a, l);
  static_assert( std::is_same<decltype(r24), long>::value, "" );
  auto r25 = atomic_fetch_sub_explicit(&v, i, mo);
  static_assert( std::is_same<decltype(r25), int>::value, "" );
  auto r26 = atomic_fetch_sub_explicit(&a, l, mo);
  static_assert( std::is_same<decltype(r26), long>::value, "" );
  auto r27 = atomic_fetch_and(&v, i);
  static_assert( std::is_same<decltype(r27), int>::value, "" );
  auto r28 = atomic_fetch_and(&a, l);
  static_assert( std::is_same<decltype(r28), long>::value, "" );
  auto r29 = atomic_fetch_and_explicit(&v, i, mo);
  static_assert( std::is_same<decltype(r29), int>::value, "" );
  auto r30 = atomic_fetch_and_explicit(&a, l, mo);
  static_assert( std::is_same<decltype(r30), long>::value, "" );
  auto r31 = atomic_fetch_or(&v, i);
  static_assert( std::is_same<decltype(r31), int>::value, "" );
  auto r32 = atomic_fetch_or(&a, l);
  static_assert( std::is_same<decltype(r32), long>::value, "" );
  auto r33 = atomic_fetch_or_explicit(&v, i, mo);
  static_assert( std::is_same<decltype(r33), int>::value, "" );
  auto r34 = atomic_fetch_or_explicit(&a, l, mo);
  static_assert( std::is_same<decltype(r34), long>::value, "" );
  auto r35 = atomic_fetch_xor(&v, i);
  static_assert( std::is_same<decltype(r35), int>::value, "" );
  auto r36 = atomic_fetch_xor(&a, l);
  static_assert( std::is_same<decltype(r36), long>::value, "" );
  auto r37 = atomic_fetch_xor_explicit(&v, i, mo);
  static_assert( std::is_same<decltype(r37), int>::value, "" );
  auto r38 = atomic_fetch_xor_explicit(&a, l, mo);
  static_assert( std::is_same<decltype(r38), long>::value, "" );
}

void
test02()
{
  volatile std::atomic<long> v;
  std::atomic<long> a;
  std::memory_order mo = std::memory_order_seq_cst;
  // Repeat tests with arguments of type different to value_type.
  const int i = 0;
  long l = 0;
  atomic_init(&v, i);
  atomic_init(&a, i);
  atomic_store(&v, i);
  atomic_store(&a, i);
  atomic_store_explicit(&v, i, mo);
  atomic_store_explicit(&a, i, mo);
  atomic_exchange(&v, i);
  atomic_exchange(&a, i);
  atomic_exchange_explicit(&v, i, mo);
  atomic_exchange_explicit(&a, i, mo);
  atomic_compare_exchange_weak(&v, &l, i);
  atomic_compare_exchange_weak(&a, &l, i);
  atomic_compare_exchange_strong(&v, &l, i);
  atomic_compare_exchange_strong(&a, &l, i);
  atomic_compare_exchange_weak_explicit(&v, &l, i, mo, mo);
  atomic_compare_exchange_weak_explicit(&a, &l, i, mo, mo);
  atomic_compare_exchange_strong_explicit(&v, &l, i, mo, mo);
  atomic_compare_exchange_strong_explicit(&a, &l, i, mo, mo);
  atomic_fetch_add(&v, i);
  atomic_fetch_add(&a, i);
  atomic_fetch_add_explicit(&v, i, mo);
  atomic_fetch_add_explicit(&a, i, mo);
  atomic_fetch_sub(&v, i);
  atomic_fetch_sub(&a, i);
  atomic_fetch_sub_explicit(&v, i, mo);
  atomic_fetch_sub_explicit(&a, i, mo);
  atomic_fetch_and(&v, i);
  atomic_fetch_and(&a, i);
  atomic_fetch_and_explicit(&v, i, mo);
  atomic_fetch_and_explicit(&a, i, mo);
  atomic_fetch_or(&v, i);
  atomic_fetch_or(&a, i);
  atomic_fetch_or_explicit(&v, i, mo);
  atomic_fetch_or_explicit(&a, i, mo);
  atomic_fetch_xor(&v, i);
  atomic_fetch_xor(&a, i);
  atomic_fetch_xor_explicit(&v, i, mo);
  atomic_fetch_xor_explicit(&a, i, mo);
}

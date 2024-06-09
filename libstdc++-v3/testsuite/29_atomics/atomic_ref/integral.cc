// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
// { dg-require-atomic-cmpxchg-word "" }
// { dg-add-options libatomic }

#include <atomic>
#include <limits.h>
#include <testsuite_hooks.h>

void
test01()
{
  int value;

  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<int> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<int>::is_always_lock_free)
      VERIFY( ok );
    a = 0;
    VERIFY( a.load() == 0 );
    VERIFY( a.load(mo) == 0 );
    a.store(1);
    VERIFY( a.load() == 1 );
    auto v = a.exchange(2);
    VERIFY( a == 2 );
    VERIFY( v == 1 );
    v = a.exchange(3, mo);
    VERIFY( a == 3 );
    VERIFY( v == 2 );

    auto expected = a.load();
    while (!a.compare_exchange_weak(expected, 4, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 4 );
    VERIFY( expected == 3 );
    expected = 1;
    ok = a.compare_exchange_weak(expected, 5, mo, mo);
    VERIFY( !ok && a.load() == 4 && expected == 4 );
    ok = a.compare_exchange_strong(expected, 5, mo, mo);
    VERIFY( ok && a.load() == 5 && expected == 4 );
    expected = 0;
    ok = a.compare_exchange_strong(expected, 3, mo, mo);
    VERIFY( !ok && a.load() == 5 && expected == 5 );

    while (!a.compare_exchange_weak(expected, 4))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 4  && expected == 5 );
    expected = a.load();
    while (!a.compare_exchange_weak(expected, 6, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 6 && expected == 4 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, -8);
    VERIFY( !ok && a.load() == 6 && expected == 6 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, 8, mo);
    VERIFY( !ok && a.load() == 6 && expected == 6 );

    ok = a.compare_exchange_strong(expected, -6);
    VERIFY( ok && a.load() == -6 && expected == 6 );
    expected = a.load();
    ok = a.compare_exchange_strong(expected, 7, mo);
    VERIFY( ok && a.load() == 7 && expected == -6 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 2);
    VERIFY( !ok && a.load() == 7 && expected == 7 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 2, mo);
    VERIFY( !ok && a.load() == 7 && expected == 7 );

    v = a.fetch_add(2);
    VERIFY( v == 7 );
    VERIFY( a == 9 );
    v = a.fetch_add(-30, mo);
    VERIFY( v == 9 );
    VERIFY( a == -21 );

    v = a.fetch_sub(3);
    VERIFY( v == -21 );
    VERIFY( a == -24 );
    v = a.fetch_sub(-41, mo);
    VERIFY( v == -24 );
    VERIFY( a == 17 );

    v = a.fetch_and(0x101);
    VERIFY( v == 17 );
    VERIFY( a == 1 );
    a = 0x17;
    v = a.fetch_and(0x23, mo);
    VERIFY( v == 0x17 );
    VERIFY( a == 3 );

    v = a.fetch_or(0x101);
    VERIFY( v == 3 );
    VERIFY( a == 0x103 );
    v = a.fetch_or(0x23, mo);
    VERIFY( v == 0x103 );
    VERIFY( a == 0x123 );

    v = a.fetch_xor(0x101);
    VERIFY( v == 0x123 );
    VERIFY( a == 0x022 );
    v = a.fetch_xor(0x123, mo);
    VERIFY( v == 0x022 );
    VERIFY( a == 0x101 );

    v = a++;
    VERIFY( v == 0x101 );
    VERIFY( a == 0x102 );
    v = a--;
    VERIFY( v == 0x102 );
    VERIFY( a == 0x101 );
    v = ++a;
    VERIFY( v == 0x102 );
    VERIFY( a == 0x102 );
    v = --a;
    VERIFY( v == 0x101 );
    VERIFY( a == 0x101 );

    v = a += -10;
    VERIFY( v == 247 );
    VERIFY( a == 247 );

    v = a -= 250;
    VERIFY( v == -3 );
    VERIFY( a == -3 );

    a = 0x17;
    v = a &= 0x102;
    VERIFY( v == 2 );
    VERIFY( a == 2 );

    v = a |= 0x101;
    VERIFY( v == 0x103 );
    VERIFY( a == 0x103 );

    v = a ^= 0x121;
    VERIFY( v == 0x022 );
    VERIFY( a == 0x022 );
  }

  VERIFY( value == 0x022 );
}

void
test02()
{
  unsigned short value;

  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<unsigned short> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<unsigned short>::is_always_lock_free)
      VERIFY( ok );
    a = 0;
    VERIFY( a.load() == 0 );
    VERIFY( a.load(mo) == 0 );
    a.store(1);
    VERIFY( a.load() == 1 );
    auto v = a.exchange(2);
    VERIFY( a == 2 );
    VERIFY( v == 1 );
    v = a.exchange(3, mo);
    VERIFY( a == 3 );
    VERIFY( v == 2 );

    auto expected = a.load();
    while (!a.compare_exchange_weak(expected, 4, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 4 );
    VERIFY( expected == 3 );
    expected = 1;
    ok = a.compare_exchange_weak(expected, 5, mo, mo);
    VERIFY( !ok && a.load() == 4 && expected == 4 );
    ok = a.compare_exchange_strong(expected, 5, mo, mo);
    VERIFY( ok && a.load() == 5 && expected == 4 );
    expected = 0;
    ok = a.compare_exchange_strong(expected, 3, mo, mo);
    VERIFY( !ok && a.load() == 5 && expected == 5 );

    while (!a.compare_exchange_weak(expected, 4))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 4  && expected == 5 );
    expected = a.load();
    while (!a.compare_exchange_weak(expected, 6, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 6 && expected == 4 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, -8);
    VERIFY( !ok && a.load() == 6 && expected == 6 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, 8, mo);
    VERIFY( !ok && a.load() == 6 && expected == 6 );

    ok = a.compare_exchange_strong(expected, -6);
    VERIFY( ok && a.load() == (unsigned short)-6 && expected == 6 );
    expected = a.load();
    ok = a.compare_exchange_strong(expected, 7, mo);
    VERIFY( ok && a.load() == 7 && expected == (unsigned short)-6 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 2);
    VERIFY( !ok && a.load() == 7 && expected == 7 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 2, mo);
    VERIFY( !ok && a.load() == 7 && expected == 7 );

    v = a.fetch_add(2);
    VERIFY( v == 7 );
    VERIFY( a == 9 );
    v = a.fetch_add(-30, mo);
    VERIFY( v == 9 );
    VERIFY( a == (unsigned short)-21 );

    v = a.fetch_sub(3);
    VERIFY( v == (unsigned short)-21 );
    VERIFY( a == (unsigned short)-24 );
    v = a.fetch_sub((unsigned short)-41, mo);
    VERIFY( v == (unsigned short)-24 );
    VERIFY( a == 17 );

    v = a.fetch_and(0x21);
    VERIFY( v == 17 );
    VERIFY( a == 1 );
    a = 0x17;
    v = a.fetch_and(0x23, mo);
    VERIFY( v == 0x17 );
    VERIFY( a == 3 );

    v = a.fetch_or(0x21);
    VERIFY( v == 3 );
    VERIFY( a == 0x23 );
    v = a.fetch_or(0x44, mo);
    VERIFY( v == 0x23 );
    VERIFY( a == 0x67 );

    v = a.fetch_xor(0x21);
    VERIFY( v == 0x67 );
    VERIFY( a == 0x46 );
    v = a.fetch_xor(0x12, mo);
    VERIFY( v == 0x46 );
    VERIFY( a == 0x54 );

    v = a++;
    VERIFY( v == 0x54 );
    VERIFY( a == 0x55 );
    v = a--;
    VERIFY( v == 0x55 );
    VERIFY( a == 0x54 );
    v = ++a;
    VERIFY( v == 0x55 );
    VERIFY( a == 0x55 );
    v = --a;
    VERIFY( v == 0x54 );
    VERIFY( a == 0x54 );

    v = a += -10;
    VERIFY( v == 0x4a );
    VERIFY( a == 0x4a );

    v = a -= 15;
    VERIFY( v == 0x3b );
    VERIFY( a == 0x3b );

    a = 0x17;
    v = a &= 0x12;
    VERIFY( v == 0x12 );
    VERIFY( a == 0x12 );

    v = a |= 0x34;
    VERIFY( v == 0x36 );
    VERIFY( a == 0x36 );

    v = a ^= 0x12;
    VERIFY( v == 0x24 );
    VERIFY( a == 0x24 );
  }

  VERIFY( value == 0x24 );
}
void
test03()
{
  int i = 0;
  std::atomic_ref<int> a0(i);
  std::atomic_ref<int> a1(i);
  std::atomic_ref<int> a2(a0);
  a0 = 42;
  VERIFY( a1 == 42 );
  VERIFY( a2 == 42 );
}

void
test04()
{
  int i = INT_MIN;
  std::atomic_ref<int> a(i);
  --a;
  VERIFY( a == INT_MAX );
  ++a;
  VERIFY( a == INT_MIN );
  a |= INT_MAX;
  VERIFY( a == -1 );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}

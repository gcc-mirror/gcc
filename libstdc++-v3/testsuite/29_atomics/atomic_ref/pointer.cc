// Copyright (C) 2019 Free Software Foundation, Inc.
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
// { dg-add-options libatomic }

#include <atomic>
#include <testsuite_hooks.h>

void
test01()
{
  long arr[10] = { };
  long* value;

  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<long*> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<long*>::is_always_lock_free)
      VERIFY( ok );
    a = arr;
    VERIFY( a.load() == arr );
    VERIFY( a.load(mo) == arr );
    a.store(arr+1);
    VERIFY( a.load() == arr+1 );
    auto v = a.exchange(arr+2);
    VERIFY( a == arr+2 );
    VERIFY( v == arr+1 );
    v = a.exchange(arr+3, mo);
    VERIFY( a == arr+3 );
    VERIFY( v == arr+2 );

    auto expected = a.load();
    while (!a.compare_exchange_weak(expected, arr+4, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == arr+4 );
    VERIFY( expected == arr+3 );
    expected = arr+1;
    ok = a.compare_exchange_weak(expected, arr+5, mo, mo);
    VERIFY( !ok && a.load() == arr+4 && expected == arr+4 );
    ok = a.compare_exchange_strong(expected, arr+5, mo, mo);
    VERIFY( ok && a.load() == arr+5 && expected == arr+4 );
    expected = nullptr;
    ok = a.compare_exchange_strong(expected, arr+3, mo, mo);
    VERIFY( !ok && a.load() == arr+5 && expected == arr+5 );

    while (!a.compare_exchange_weak(expected, arr+4))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == arr+4  && expected == arr+5 );
    expected = a.load();
    while (!a.compare_exchange_weak(expected, arr+6, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == arr+6 && expected == arr+4 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, arr+8);
    VERIFY( !ok && a.load() == arr+6 && expected == arr+6 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, arr+8, mo);
    VERIFY( !ok && a.load() == arr+6 && expected == arr+6 );

    ok = a.compare_exchange_strong(expected, arr+5);
    VERIFY( ok && a.load() == arr+5 && expected == arr+6 );
    expected = a.load();
    ok = a.compare_exchange_strong(expected, arr+7, mo);
    VERIFY( ok && a.load() == arr+7 && expected == arr+5 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, arr+2);
    VERIFY( !ok && a.load() == arr+7 && expected == arr+7 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, arr+2, mo);
    VERIFY( !ok && a.load() == arr+7 && expected == arr+7 );

    v = a.fetch_add(2);
    VERIFY( v == arr+7 );
    VERIFY( a == arr+9 );
    v = a.fetch_add(-3, mo);
    VERIFY( v == arr+9 );
    VERIFY( a == arr+6 );

    v = a.fetch_sub(3);
    VERIFY( v == arr+6 );
    VERIFY( a == arr+3 );
    v = a.fetch_sub(2, mo);
    VERIFY( v == arr+3 );
    VERIFY( a == arr+1 );

    v = a += 5;
    VERIFY( v == arr+6 );
    VERIFY( a == arr+6 );

    v = a -= 5;
    VERIFY( v == arr+1 );
    VERIFY( a == arr+1 );
  }

  VERIFY( value == arr+1 );
}

void
test02()
{
  char arr[10] = { };
  char* value;

  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<char*> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<char*>::is_always_lock_free)
      VERIFY( ok );
    a = arr;
    VERIFY( a.load() == arr );
    a.store(arr+3);
    VERIFY( a.load(mo) == arr+3 );
    a.store(arr+1, mo);
    VERIFY( a.load() == arr+1 );
    auto v = a.exchange(arr+2);
    VERIFY( a == arr+2 );
    VERIFY( v == arr+1 );
    v = a.exchange(arr+3, mo);
    VERIFY( a == arr+3 );
    VERIFY( v == arr+2 );

    auto expected = a.load();
    while (!a.compare_exchange_weak(expected, arr+4, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == arr+4 );
    VERIFY( expected == arr+3 );
    expected = arr+1;
    ok = a.compare_exchange_weak(expected, arr+5, mo, mo);
    VERIFY( !ok && a.load() == arr+4 && expected == arr+4 );
    ok = a.compare_exchange_strong(expected, arr+5, mo, mo);
    VERIFY( ok && a.load() == arr+5 && expected == arr+4 );
    expected = nullptr;
    ok = a.compare_exchange_strong(expected, arr+3, mo, mo);
    VERIFY( !ok && a.load() == arr+5 && expected == arr+5 );

    while (!a.compare_exchange_weak(expected, arr+4))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == arr+4  && expected == arr+5 );
    expected = a.load();
    while (!a.compare_exchange_weak(expected, arr+6, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == arr+6 && expected == arr+4 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, arr+8);
    VERIFY( !ok && a.load() == arr+6 && expected == arr+6 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, arr+8, mo);
    VERIFY( !ok && a.load() == arr+6 && expected == arr+6 );

    ok = a.compare_exchange_strong(expected, arr+5);
    VERIFY( ok && a.load() == arr+5 && expected == arr+6 );
    expected = a.load();
    ok = a.compare_exchange_strong(expected, arr+7, mo);
    VERIFY( ok && a.load() == arr+7 && expected == arr+5 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, arr+2);
    VERIFY( !ok && a.load() == arr+7 && expected == arr+7 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, arr+2, mo);
    VERIFY( !ok && a.load() == arr+7 && expected == arr+7 );

    v = a.fetch_add(2);
    VERIFY( v == arr+7 );
    VERIFY( a == arr+9 );
    v = a.fetch_add(-3, mo);
    VERIFY( v == arr+9 );
    VERIFY( a == arr+6 );

    v = a.fetch_sub(3);
    VERIFY( v == arr+6 );
    VERIFY( a == arr+3 );
    v = a.fetch_sub(2, mo);
    VERIFY( v == arr+3 );
    VERIFY( a == arr+1 );

    v = a += 5;
    VERIFY( v == arr+6 );
    VERIFY( a == arr+6 );

    v = a -= 5;
    VERIFY( v == arr+1 );
    VERIFY( a == arr+1 );
  }

  VERIFY( value == arr+1 );
}

void
test03()
{
  int i = 0;
  int* ptr = 0;
  std::atomic_ref<int*> a0(ptr);
  std::atomic_ref<int*> a1(ptr);
  std::atomic_ref<int*> a2(a0);
  a0 = &i;
  VERIFY( a1 == &i );
  VERIFY( a2 == &i );
}

int
main()
{
  test01();
  test02();
  test03();
}

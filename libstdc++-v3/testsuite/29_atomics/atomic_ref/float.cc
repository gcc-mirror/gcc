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

#include <atomic>
#include <testsuite_hooks.h>

void
test01()
{
  float value;
  if constexpr (std::atomic_ref<float>::is_always_lock_free)
  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<float> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<float>::is_always_lock_free)
      VERIFY( ok );
    a = 1.6f;
    VERIFY( a.load() == 1.6f );
    a.store(0.8f);
    VERIFY( a.load(mo) == 0.8f );
    a.store(3.2f, mo);
    VERIFY( a.load() == 3.2f );
    auto v = a.exchange(6.4f);
    VERIFY( a == 6.4f );
    VERIFY( v == 3.2f );
    v = a.exchange(1.28f, mo);
    VERIFY( a == 1.28f );
    VERIFY( v == 6.4f );

    auto expected = a.load();
    while (!a.compare_exchange_weak(expected, 25.6f, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 25.6f );
    VERIFY( expected == 1.28f );
    expected = 3.2f;
    ok = a.compare_exchange_weak(expected, 51.2f, mo, mo);
    VERIFY( !ok && a.load() == 25.6f && expected == 25.6f );
    ok = a.compare_exchange_strong(expected, 51.2f, mo, mo);
    VERIFY( ok && a.load() == 51.2f && expected == 25.6f );
    expected = 0.0f;
    ok = a.compare_exchange_strong(expected, 1.28f, mo, mo);
    VERIFY( !ok && a.load() == 51.2f && expected == 51.2f );

    while (!a.compare_exchange_weak(expected, 25.6f))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 25.6f  && expected == 51.2f );
    expected = a.load();
    while (!a.compare_exchange_weak(expected, 10.24f, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 10.24f && expected == 25.6f );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, 40.96f);
    VERIFY( !ok && a.load() == 10.24f && expected == 10.24f );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, 40.96f, mo);
    VERIFY( !ok && a.load() == 10.24f && expected == 10.24f );

    ok = a.compare_exchange_strong(expected, 1.024f);
    VERIFY( ok && a.load() == 1.024f && expected == 10.24f );
    expected = a.load();
    ok = a.compare_exchange_strong(expected, 204.8f, mo);
    VERIFY( ok && a.load() == 204.8f && expected == 1.024f );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 6.4f);
    VERIFY( !ok && a.load() == 204.8f && expected == 204.8f );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 6.4f, mo);
    VERIFY( !ok && a.load() == 204.8f && expected == 204.8f );

    v = a.fetch_add(3.2f);
    VERIFY( v == 204.8f );
    VERIFY( a == 208.0f );
    v = a.fetch_add(-8.5f, mo);
    VERIFY( v == 208.0f );
    VERIFY( a == 199.5f );

    v = a.fetch_sub(109.5f);
    VERIFY( v == 199.5f );
    VERIFY( a == 90.0f );
    v = a.fetch_sub(2, mo);
    VERIFY( v == 90.0f );
    VERIFY( a == 88.0f );

    v = a += 5.0f;
    VERIFY( v == 93.0f );
    VERIFY( a == 93.0f );

    v = a -= 6.5f;
    VERIFY( v == 86.5f );
    VERIFY( a == 86.5f );
  }

  if constexpr (std::atomic_ref<float>::is_always_lock_free)
    VERIFY( value == 86.5f );
}

void
test02()
{
  double value;
  if constexpr (std::atomic_ref<double>::is_always_lock_free)
  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<double> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<double>::is_always_lock_free)
      VERIFY( ok );
    a = 1.6;
    VERIFY( a.load() == 1.6 );
    a.store(0.8);
    VERIFY( a.load(mo) == 0.8 );
    a.store(3.2, mo);
    VERIFY( a.load() == 3.2 );
    auto v = a.exchange(6.4);
    VERIFY( a == 6.4 );
    VERIFY( v == 3.2 );
    v = a.exchange(1.28, mo);
    VERIFY( a == 1.28 );
    VERIFY( v == 6.4 );

    auto expected = a.load();
    while (!a.compare_exchange_weak(expected, 25.6, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 25.6 );
    VERIFY( expected == 1.28 );
    expected = 3.2;
    ok = a.compare_exchange_weak(expected, 51.2, mo, mo);
    VERIFY( !ok && a.load() == 25.6 && expected == 25.6 );
    ok = a.compare_exchange_strong(expected, 51.2, mo, mo);
    VERIFY( ok && a.load() == 51.2 && expected == 25.6 );
    expected = 0.0;
    ok = a.compare_exchange_strong(expected, 1.28, mo, mo);
    VERIFY( !ok && a.load() == 51.2 && expected == 51.2 );

    while (!a.compare_exchange_weak(expected, 25.6))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 25.6  && expected == 51.2 );
    expected = a.load();
    while (!a.compare_exchange_weak(expected, 10.24, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 10.24 && expected == 25.6 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, 40.96);
    VERIFY( !ok && a.load() == 10.24 && expected == 10.24 );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, 40.96, mo);
    VERIFY( !ok && a.load() == 10.24 && expected == 10.24 );

    ok = a.compare_exchange_strong(expected, 1.024);
    VERIFY( ok && a.load() == 1.024 && expected == 10.24 );
    expected = a.load();
    ok = a.compare_exchange_strong(expected, 204.8, mo);
    VERIFY( ok && a.load() == 204.8 && expected == 1.024 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 6.4);
    VERIFY( !ok && a.load() == 204.8 && expected == 204.8 );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 6.4, mo);
    VERIFY( !ok && a.load() == 204.8 && expected == 204.8 );

    v = a.fetch_add(3.2);
    VERIFY( v == 204.8 );
    VERIFY( a == 208.0 );
    v = a.fetch_add(-8.5, mo);
    VERIFY( v == 208.0 );
    VERIFY( a == 199.5 );

    v = a.fetch_sub(109.5);
    VERIFY( v == 199.5 );
    VERIFY( a == 90.0 );
    v = a.fetch_sub(2, mo);
    VERIFY( v == 90.0 );
    VERIFY( a == 88.0 );

    v = a += 5.0;
    VERIFY( v == 93.0 );
    VERIFY( a == 93.0 );

    v = a -= 6.5;
    VERIFY( v == 86.5 );
    VERIFY( a == 86.5 );
  }

  if constexpr (std::atomic_ref<double>::is_always_lock_free)
    VERIFY( value == 86.5 );
}

void
test03()
{
  long double value;
  if constexpr (std::atomic_ref<long double>::is_always_lock_free)
  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<long double> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<long double>::is_always_lock_free)
      VERIFY( ok );
    a = 1.6l;
    VERIFY( a.load() == 1.6l );
    a.store(0.8l);
    VERIFY( a.load(mo) == 0.8l );
    a.store(3.2l, mo);
    VERIFY( a.load() == 3.2l );
    auto v = a.exchange(6.4l);
    VERIFY( a == 6.4l );
    VERIFY( v == 3.2l );
    v = a.exchange(1.28l, mo);
    VERIFY( a == 1.28l );
    VERIFY( v == 6.4l );

    auto expected = a.load();
    while (!a.compare_exchange_weak(expected, 25.6l, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 25.6l );
    VERIFY( expected == 1.28l );
    expected = 3.2l;
    ok = a.compare_exchange_weak(expected, 51.2l, mo, mo);
    VERIFY( !ok && a.load() == 25.6l && expected == 25.6l );
    ok = a.compare_exchange_strong(expected, 51.2l, mo, mo);
    VERIFY( ok && a.load() == 51.2l && expected == 25.6l );
    expected = 0.0l;
    ok = a.compare_exchange_strong(expected, 1.28l, mo, mo);
    VERIFY( !ok && a.load() == 51.2l && expected == 51.2l );

    while (!a.compare_exchange_weak(expected, 25.6l))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 25.6l  && expected == 51.2l );
    expected = a.load();
    while (!a.compare_exchange_weak(expected, 10.24l, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == 10.24l && expected == 25.6l );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, 40.96l);
    VERIFY( !ok && a.load() == 10.24l && expected == 10.24l );
    expected = a.load() + 1;
    ok = a.compare_exchange_weak(expected, 40.96l, mo);
    VERIFY( !ok && a.load() == 10.24l && expected == 10.24l );

    ok = a.compare_exchange_strong(expected, 1.024l);
    VERIFY( ok && a.load() == 1.024l && expected == 10.24l );
    expected = a.load();
    ok = a.compare_exchange_strong(expected, 204.8l, mo);
    VERIFY( ok && a.load() == 204.8l && expected == 1.024l );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 6.4l);
    VERIFY( !ok && a.load() == 204.8l && expected == 204.8l );
    expected = a.load() + 1;
    ok = a.compare_exchange_strong(expected, 6.4l, mo);
    VERIFY( !ok && a.load() == 204.8l && expected == 204.8l );

    a = 0.5l;
    v = a.fetch_add(0.5l);
    VERIFY( v == 0.5l );
    VERIFY( a == 1.0l );
    v = a.fetch_add(-0.2l, mo);
    VERIFY( v == 1.0l );
    VERIFY( a == 0.8l );

    v = a.fetch_sub(0.4l);
    VERIFY( v == 0.8l );
    VERIFY( a == 0.4l );
    v = a.fetch_sub(-0.4l, mo);
    VERIFY( v == 0.4l );
    VERIFY( a == 0.8l );

    v = a += .8l;
    VERIFY( v == 1.6l );
    VERIFY( a == 1.6l );

    v = a -= 0.6l;
    VERIFY( v == 1.0l );
    VERIFY( a == 1.0l );
  }

  if constexpr (std::atomic_ref<long double>::is_always_lock_free)
    VERIFY( value == 1.0l );
}

void
test04()
{
  if constexpr (std::atomic_ref<float>::is_always_lock_free)
  {
    float i = 0;
    float* ptr = 0;
    std::atomic_ref<float*> a0(ptr);
    std::atomic_ref<float*> a1(ptr);
    std::atomic_ref<float*> a2(a0);
    a0 = &i;
    VERIFY( a1 == &i );
    VERIFY( a2 == &i );
  }
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}

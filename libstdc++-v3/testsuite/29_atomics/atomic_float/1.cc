// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <atomic>
#include <testsuite_hooks.h>

void
test01()
{
  const auto mo = std::memory_order_relaxed;
  bool ok;
  float expected;

  if constexpr (std::atomic<float>::is_always_lock_free)
  {
    std::atomic<float> a0;
    std::atomic<float> a1(1.0f);
    ok = a0.is_lock_free();
    a0 = a1.load();
    VERIFY( a0.load() == a1.load() );
    VERIFY( a0.load(mo) == a0.load() );
    a0.store(0.5f);
    a1.store(0.5f, mo);
    VERIFY( a0.load() == a1.load() );
    auto f0 = a0.exchange(12.5f);
    auto f1 = a1.exchange(12.5f, mo);
    VERIFY( a0 == 12.5f );
    VERIFY( a0.load() == a1.load() );
    VERIFY( f0 == 0.5f );
    VERIFY( f0 == f1 );

    expected = 12.5f;
    while (!a0.compare_exchange_weak(expected, 1.6f, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == 1.6f );
    VERIFY( expected == 12.5f );
    expected = 1.5f;
    ok = a1.compare_exchange_weak(expected, 1.6f, mo, mo);
    VERIFY( !ok && a1.load() == 12.5f && expected == 12.5f );
    VERIFY( expected == 12.5f );
    expected = 1.6f;
    ok = a0.compare_exchange_strong(expected, 3.2f, mo, mo);
    VERIFY( ok && a0.load() == 3.2f );
    VERIFY( expected == 1.6f );
    expected = 1.5f;
    ok = a1.compare_exchange_strong(expected, 3.2f, mo, mo);
    VERIFY( !ok && a1.load() == 12.5f && expected == 12.5f );

    expected = 3.2f;
    while (!a0.compare_exchange_weak(expected, .64f))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == .64f );
    expected = 12.5f;
    while (!a1.compare_exchange_weak(expected, 1.6f, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a1.load() == 1.6f );
    expected = 0.5f;
    ok = a0.compare_exchange_weak(expected, 3.2f);
    VERIFY( !ok && a0.load() == .64f && expected == .64f );
    expected = 0.5f;
    ok = a1.compare_exchange_weak(expected, 3.2f, mo);
    VERIFY( !ok && a1.load() == 1.6f && expected == 1.6f );

    expected = .64f;
    ok = a0.compare_exchange_strong(expected, 12.8f);
    VERIFY( ok && a0.load() == 12.8f );
    expected = 1.6f;
    ok = a1.compare_exchange_strong(expected, 2.56f, mo);
    VERIFY( ok && a1.load() == 2.56f );
    expected = 0.5f;
    ok = a0.compare_exchange_strong(expected, 3.2f);
    VERIFY( !ok && a0.load() == 12.8f && expected == 12.8f );
    expected = 0.5f;
    ok = a1.compare_exchange_strong(expected, 3.2f, mo);
    VERIFY( !ok && a1.load() == 2.56f && expected == 2.56f );

    f0 = a0.fetch_add(1.2f);
    VERIFY( f0 == 12.8f );
    VERIFY( a0 == 14.0f );
    f1 = a1.fetch_add(2.4f, mo);
    VERIFY( f1 == 2.56f );
    VERIFY( a1 == 4.96f );

    f0 = a0.fetch_sub(1.2f);
    VERIFY( f0 == 14.0f );
    VERIFY( a0 == 12.8f );
    f1 = a1.fetch_sub(3.5f, mo);
    VERIFY( f1 == 4.96f );
    VERIFY( a1 == 1.46f );

    f0 = a0 += 1.2f;
    VERIFY( f0 == 14.0f );
    VERIFY( a0 == 14.0f );

    f0 = a0 -= 0.8f;
    VERIFY( f0 == 13.2f );
    VERIFY( a0 == 13.2f );
  }

  // Repeat for volatile std::atomic<float>
  if constexpr (std::atomic<float>::is_always_lock_free)
  {
    volatile std::atomic<float> a0;
    volatile std::atomic<float> a1(1.0f);
    ok = a0.is_lock_free();
    a0 = a1.load();
    VERIFY( a0.load() == a1.load() );
    VERIFY( a0.load(mo) == a0.load() );
    a0.store(0.5f);
    a1.store(0.5f, mo);
    VERIFY( a0.load() == a1.load() );
    auto f0 = a0.exchange(12.5f);
    auto f1 = a1.exchange(12.5f, mo);
    VERIFY( a0 == 12.5f );
    VERIFY( a0.load() == a1.load() );
    VERIFY( f0 == 0.5f );
    VERIFY( f0 == f1 );

    expected = 12.5f;
    while (!a0.compare_exchange_weak(expected, 1.6f, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == 1.6f );
    VERIFY( expected == 12.5f );
    expected = 1.5f;
    ok = a1.compare_exchange_weak(expected, 1.6f, mo, mo);
    VERIFY( !ok && a1.load() == 12.5f && expected == 12.5f );
    VERIFY( expected == 12.5f );
    expected = 1.6f;
    ok = a0.compare_exchange_strong(expected, 3.2f, mo, mo);
    VERIFY( ok && a0.load() == 3.2f );
    VERIFY( expected == 1.6f );
    expected = 1.5f;
    ok = a1.compare_exchange_strong(expected, 3.2f, mo, mo);
    VERIFY( !ok && a1.load() == 12.5f && expected == 12.5f );

    expected = 3.2f;
    while (!a0.compare_exchange_weak(expected, .64f))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == .64f );
    expected = 12.5f;
    while (!a1.compare_exchange_weak(expected, 1.6f, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a1.load() == 1.6f );
    expected = 0.5f;
    ok = a0.compare_exchange_weak(expected, 3.2f);
    VERIFY( !ok && a0.load() == .64f && expected == .64f );
    expected = 0.5f;
    ok = a1.compare_exchange_weak(expected, 3.2f, mo);
    VERIFY( !ok && a1.load() == 1.6f && expected == 1.6f );

    expected = .64f;
    ok = a0.compare_exchange_strong(expected, 12.8f);
    VERIFY( ok && a0.load() == 12.8f );
    expected = 1.6f;
    ok = a1.compare_exchange_strong(expected, 2.56f, mo);
    VERIFY( ok && a1.load() == 2.56f );
    expected = 0.5f;
    ok = a0.compare_exchange_strong(expected, 3.2f);
    VERIFY( !ok && a0.load() == 12.8f && expected == 12.8f );
    expected = 0.5f;
    ok = a1.compare_exchange_strong(expected, 3.2f, mo);
    VERIFY( !ok && a1.load() == 2.56f && expected == 2.56f );

    f0 = a0.fetch_add(1.2f);
    VERIFY( f0 == 12.8f );
    VERIFY( a0 == 14.0f );
    f1 = a1.fetch_add(2.4f, mo);
    VERIFY( f1 == 2.56f );
    VERIFY( a1 == 4.96f );

    f0 = a0.fetch_sub(1.2f);
    VERIFY( f0 == 14.0f );
    VERIFY( a0 == 12.8f );
    f1 = a1.fetch_sub(3.5f, mo);
    VERIFY( f1 == 4.96f );
    VERIFY( a1 == 1.46f );

    f0 = a0 += 1.2f;
    VERIFY( f0 == 14.0f );
    VERIFY( a0 == 14.0f );

    f0 = a0 -= 0.8f;
    VERIFY( f0 == 13.2f );
    VERIFY( a0 == 13.2f );
  }
}

void
test02()
{
  const auto mo = std::memory_order_relaxed;
  bool ok;
  double expected;

  if constexpr (std::atomic<double>::is_always_lock_free)
  {
    std::atomic<double> a0;
    std::atomic<double> a1(1.0);
    ok = a0.is_lock_free();
    a0 = a1.load();
    VERIFY( a0.load() == a1.load() );
    VERIFY( a0.load(mo) == a0.load() );
    a0.store(0.5);
    a1.store(0.5, mo);
    VERIFY( a0.load() == a1.load() );
    auto f0 = a0.exchange(12.5);
    auto f1 = a1.exchange(12.5, mo);
    VERIFY( a0 == 12.5 );
    VERIFY( a0.load() == a1.load() );
    VERIFY( f0 == 0.5 );
    VERIFY( f0 == f1 );

    expected = 12.5;
    while (!a0.compare_exchange_weak(expected, 1.6, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == 1.6 );
    VERIFY( expected == 12.5 );
    expected = 1.5;
    ok = a1.compare_exchange_weak(expected, 1.6, mo, mo);
    VERIFY( !ok && a1.load() == 12.5 && expected == 12.5 );
    VERIFY( expected == 12.5 );
    expected = 1.6;
    ok = a0.compare_exchange_strong(expected, 3.2, mo, mo);
    VERIFY( ok && a0.load() == 3.2 );
    VERIFY( expected == 1.6 );
    expected = 1.5;
    ok = a1.compare_exchange_strong(expected, 3.2, mo, mo);
    VERIFY( !ok && a1.load() == 12.5 && expected == 12.5 );

    expected = 3.2;
    while (!a0.compare_exchange_weak(expected, .64))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == .64 );
    expected = 12.5;
    while (!a1.compare_exchange_weak(expected, 1.6, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a1.load() == 1.6 );
    expected = 0.5;
    ok = a0.compare_exchange_weak(expected, 3.2);
    VERIFY( !ok && a0.load() == .64 && expected == .64 );
    expected = 0.5;
    ok = a1.compare_exchange_weak(expected, 3.2, mo);
    VERIFY( !ok && a1.load() == 1.6 && expected == 1.6 );

    expected = .64;
    ok = a0.compare_exchange_strong(expected, 12.8);
    VERIFY( ok && a0.load() == 12.8 );
    expected = 1.6;
    ok = a1.compare_exchange_strong(expected, 2.56, mo);
    VERIFY( ok && a1.load() == 2.56 );
    expected = 0.5;
    ok = a0.compare_exchange_strong(expected, 3.2);
    VERIFY( !ok && a0.load() == 12.8 && expected == 12.8 );
    expected = 0.5;
    ok = a1.compare_exchange_strong(expected, 3.2, mo);
    VERIFY( !ok && a1.load() == 2.56 && expected == 2.56 );

    f0 = a0.fetch_add(1.2);
    VERIFY( f0 == 12.8 );
    VERIFY( a0 == 14.0 );
    f1 = a1.fetch_add(2.4, mo);
    VERIFY( f1 == 2.56 );
    VERIFY( a1 == 4.96 );

    f0 = a0.fetch_sub(1.2);
    VERIFY( f0 == 14.0 );
    VERIFY( a0 == 12.8 );
    f1 = a1.fetch_sub(3.5, mo);
    VERIFY( f1 == 4.96 );
    VERIFY( a1 == 1.46 );

    f0 = a0 += 1.2;
    VERIFY( f0 == 14.0 );
    VERIFY( a0 == 14.0 );

    f0 = a0 -= 0.8;
    VERIFY( f0 == 13.2 );
    VERIFY( a0 == 13.2 );
  }

  // Repeat for volatile std::atomic<double>
  if constexpr (std::atomic<double>::is_always_lock_free)
  {
    volatile std::atomic<double> a0;
    volatile std::atomic<double> a1(1.0);
    ok = a0.is_lock_free();
    a0 = a1.load();
    VERIFY( a0.load() == a1.load() );
    VERIFY( a0.load(mo) == a0.load() );
    a0.store(0.5);
    a1.store(0.5, mo);
    VERIFY( a0.load() == a1.load() );
    auto f0 = a0.exchange(12.5);
    auto f1 = a1.exchange(12.5, mo);
    VERIFY( a0 == 12.5 );
    VERIFY( a0.load() == a1.load() );
    VERIFY( f0 == 0.5 );
    VERIFY( f0 == f1 );

    expected = 12.5;
    while (!a0.compare_exchange_weak(expected, 1.6, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == 1.6 );
    VERIFY( expected == 12.5 );
    expected = 1.5;
    ok = a1.compare_exchange_weak(expected, 1.6, mo, mo);
    VERIFY( !ok && a1.load() == 12.5 && expected == 12.5 );
    VERIFY( expected == 12.5 );
    expected = 1.6;
    ok = a0.compare_exchange_strong(expected, 3.2, mo, mo);
    VERIFY( ok && a0.load() == 3.2 );
    VERIFY( expected == 1.6 );
    expected = 1.5;
    ok = a1.compare_exchange_strong(expected, 3.2, mo, mo);
    VERIFY( !ok && a1.load() == 12.5 && expected == 12.5 );

    expected = 3.2;
    while (!a0.compare_exchange_weak(expected, .64))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == .64 );
    expected = 12.5;
    while (!a1.compare_exchange_weak(expected, 1.6, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a1.load() == 1.6 );
    expected = 0.5;
    ok = a0.compare_exchange_weak(expected, 3.2);
    VERIFY( !ok && a0.load() == .64 && expected == .64 );
    expected = 0.5;
    ok = a1.compare_exchange_weak(expected, 3.2, mo);
    VERIFY( !ok && a1.load() == 1.6 && expected == 1.6 );

    expected = .64;
    ok = a0.compare_exchange_strong(expected, 12.8);
    VERIFY( ok && a0.load() == 12.8 );
    expected = 1.6;
    ok = a1.compare_exchange_strong(expected, 2.56, mo);
    VERIFY( ok && a1.load() == 2.56 );
    expected = 0.5;
    ok = a0.compare_exchange_strong(expected, 3.2);
    VERIFY( !ok && a0.load() == 12.8 && expected == 12.8 );
    expected = 0.5;
    ok = a1.compare_exchange_strong(expected, 3.2, mo);
    VERIFY( !ok && a1.load() == 2.56 && expected == 2.56 );

    f0 = a0.fetch_add(1.2);
    VERIFY( f0 == 12.8 );
    VERIFY( a0 == 14.0 );
    f1 = a1.fetch_add(2.4, mo);
    VERIFY( f1 == 2.56 );
    VERIFY( a1 == 4.96 );

    f0 = a0.fetch_sub(1.2);
    VERIFY( f0 == 14.0 );
    VERIFY( a0 == 12.8 );
    f1 = a1.fetch_sub(3.5, mo);
    VERIFY( f1 == 4.96 );
    VERIFY( a1 == 1.46 );

    f0 = a0 += 1.2;
    VERIFY( f0 == 14.0 );
    VERIFY( a0 == 14.0 );

    f0 = a0 -= 0.8;
    VERIFY( f0 == 13.2 );
    VERIFY( a0 == 13.2 );
  }
}

void
test03()
{
  const auto mo = std::memory_order_relaxed;
  bool ok;
  long double expected;

  if constexpr (std::atomic<long double>::is_always_lock_free)
  {
    std::atomic<long double> a0;
    std::atomic<long double> a1(1.0l);
    ok = a0.is_lock_free();
    a0 = a1.load();
    VERIFY( a0.load() == a1.load() );
    VERIFY( a0.load(mo) == a0.load() );
    a0.store(0.5l);
    a1.store(0.5l, mo);
    VERIFY( a0.load() == a1.load() );
    auto f0 = a0.exchange(12.5l);
    auto f1 = a1.exchange(12.5l, mo);
    VERIFY( a0 == 12.5l );
    VERIFY( a0.load() == a1.load() );
    VERIFY( f0 == 0.5l );
    VERIFY( f0 == f1 );

    expected = 12.5l;
    while (!a0.compare_exchange_weak(expected, 1.6l, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == 1.6l );
    VERIFY( expected == 12.5l );
    expected = 1.5l;
    ok = a1.compare_exchange_weak(expected, 1.6l, mo, mo);
    VERIFY( !ok && a1.load() == 12.5l && expected == 12.5l );
    VERIFY( expected == 12.5l );
    expected = 1.6l;
    ok = a0.compare_exchange_strong(expected, 3.2l, mo, mo);
    VERIFY( ok && a0.load() == 3.2l );
    VERIFY( expected == 1.6l );
    expected = 1.5l;
    ok = a1.compare_exchange_strong(expected, 3.2l, mo, mo);
    VERIFY( !ok && a1.load() == 12.5l && expected == 12.5l );

    expected = 3.2l;
    while (!a0.compare_exchange_weak(expected, .64l))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == .64l );
    expected = 12.5l;
    while (!a1.compare_exchange_weak(expected, 1.6l, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a1.load() == 1.6l );
    expected = 0.5l;
    ok = a0.compare_exchange_weak(expected, 3.2l);
    VERIFY( !ok && a0.load() == .64l && expected == .64l );
    expected = 0.5l;
    ok = a1.compare_exchange_weak(expected, 3.2l, mo);
    VERIFY( !ok && a1.load() == 1.6l && expected == 1.6l );

    expected = .64l;
    ok = a0.compare_exchange_strong(expected, 12.8l);
    VERIFY( ok && a0.load() == 12.8l );
    expected = 1.6l;
    ok = a1.compare_exchange_strong(expected, 2.56l, mo);
    VERIFY( ok && a1.load() == 2.56l );
    expected = 0.5l;
    ok = a0.compare_exchange_strong(expected, 3.2l);
    VERIFY( !ok && a0.load() == 12.8l && expected == 12.8l );
    expected = 0.5l;
    ok = a1.compare_exchange_strong(expected, 3.2l, mo);
    VERIFY( !ok && a1.load() == 2.56l && expected == 2.56l );

    f0 = a0.fetch_add(1.2l);
    VERIFY( f0 == 12.8l );
    VERIFY( a0 == 14.0l );
    f1 = a1.fetch_add(2.4l, mo);
    VERIFY( f1 == 2.56l );
    VERIFY( a1 == 4.96l );

    f0 = a0.fetch_sub(1.2l);
    VERIFY( f0 == 14.0l );
    VERIFY( a0 == 12.8l );
    f1 = a1.fetch_sub(3.5l, mo);
    VERIFY( f1 == 4.96l );
    VERIFY( a1 == 1.46l );

    f0 = a0 += 1.2l;
    VERIFY( f0 == 14.0l );
    VERIFY( a0 == 14.0l );

    f0 = a0 -= 0.8l;
    VERIFY( f0 == 13.2l );
    VERIFY( a0 == 13.2l );
  }

  // Repeat for volatile std::atomic<long double>
  if constexpr (std::atomic<long double>::is_always_lock_free)
  {
    volatile std::atomic<long double> a0;
    volatile std::atomic<long double> a1(1.0l);
    ok = a0.is_lock_free();
    a0 = a1.load();
    VERIFY( a0.load() == a1.load() );
    VERIFY( a0.load(mo) == a0.load() );
    a0.store(0.5l);
    a1.store(0.5l, mo);
    VERIFY( a0.load() == a1.load() );
    auto f0 = a0.exchange(12.5l);
    auto f1 = a1.exchange(12.5l, mo);
    VERIFY( a0 == 12.5l );
    VERIFY( a0.load() == a1.load() );
    VERIFY( f0 == 0.5l );
    VERIFY( f0 == f1 );

    expected = 12.5l;
    while (!a0.compare_exchange_weak(expected, 1.6l, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == 1.6l );
    VERIFY( expected == 12.5l );
    expected = 1.5l;
    ok = a1.compare_exchange_weak(expected, 1.6l, mo, mo);
    VERIFY( !ok && a1.load() == 12.5l && expected == 12.5l );
    VERIFY( expected == 12.5l );
    expected = 1.6l;
    ok = a0.compare_exchange_strong(expected, 3.2l, mo, mo);
    VERIFY( ok && a0.load() == 3.2l );
    VERIFY( expected == 1.6l );
    expected = 1.5l;
    ok = a1.compare_exchange_strong(expected, 3.2l, mo, mo);
    VERIFY( !ok && a1.load() == 12.5l && expected == 12.5l );

    expected = 3.2l;
    while (!a0.compare_exchange_weak(expected, .64l))
    { /* weak form can fail spuriously */ }
    VERIFY( a0.load() == .64l );
    expected = 12.5l;
    while (!a1.compare_exchange_weak(expected, 1.6l, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( a1.load() == 1.6l );
    expected = 0.5l;
    ok = a0.compare_exchange_weak(expected, 3.2l);
    VERIFY( !ok && a0.load() == .64l && expected == .64l );
    expected = 0.5l;
    ok = a1.compare_exchange_weak(expected, 3.2l, mo);
    VERIFY( !ok && a1.load() == 1.6l && expected == 1.6l );

    expected = .64l;
    ok = a0.compare_exchange_strong(expected, 12.8l);
    VERIFY( ok && a0.load() == 12.8l );
    expected = 1.6l;
    ok = a1.compare_exchange_strong(expected, 2.56l, mo);
    VERIFY( ok && a1.load() == 2.56l );
    expected = 0.5l;
    ok = a0.compare_exchange_strong(expected, 3.2l);
    VERIFY( !ok && a0.load() == 12.8l && expected == 12.8l );
    expected = 0.5l;
    ok = a1.compare_exchange_strong(expected, 3.2l, mo);
    VERIFY( !ok && a1.load() == 2.56l && expected == 2.56l );

    f0 = a0.fetch_add(1.2l);
    VERIFY( f0 == 12.8l );
    VERIFY( a0 == 14.0l );
    f1 = a1.fetch_add(2.4l, mo);
    VERIFY( f1 == 2.56l );
    VERIFY( a1 == 4.96l );

    f0 = a0.fetch_sub(1.2l);
    VERIFY( f0 == 14.0l );
    VERIFY( a0 == 12.8l );
    f1 = a1.fetch_sub(3.5l, mo);
    VERIFY( f1 == 4.96l );
    VERIFY( a1 == 1.46l );

    f0 = a0 += 1.2l;
    VERIFY( f0 == 14.0l );
    VERIFY( a0 == 14.0l );

    f0 = a0 -= 0.8l;
    VERIFY( f0 == 13.2l );
    VERIFY( a0 == 13.2l );
  }
}

int
main()
{
  test01();
  test02();
  test03();
}

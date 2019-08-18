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
#include <limits.h>
#include <testsuite_hooks.h>

struct X
{
  X() = default;
  X(int i) : i(i) { }
  bool operator==(int rhs) const { return i == rhs; }
  int i;
};

void
test01()
{
  X value;

  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<X> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<X>::is_always_lock_free)
      VERIFY( ok );
    a = X{};
    VERIFY( a.load() == 0 );
    VERIFY( a.load(mo) == 0 );
    a.store(1);
    VERIFY( a.load() == 1 );
    auto v = a.exchange(2);
    VERIFY( a.load() == 2 );
    VERIFY( v == 1 );
    v = a.exchange(3, mo);
    VERIFY( a.load() == 3 );
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
    expected = a.load();
    expected.i += 1;
    ok = a.compare_exchange_weak(expected, -8);
    VERIFY( !ok && a.load() == 6 && expected == 6 );
    expected = a.load();
    expected.i += 1;
    ok = a.compare_exchange_weak(expected, 8, mo);
    VERIFY( !ok && a.load() == 6 && expected == 6 );

    ok = a.compare_exchange_strong(expected, -6);
    VERIFY( ok && a.load() == -6 && expected == 6 );
    expected = a.load();
    ok = a.compare_exchange_strong(expected, 7, mo);
    VERIFY( ok && a.load() == 7 && expected == -6 );
    expected = a.load();
    expected.i += 1;
    ok = a.compare_exchange_strong(expected, 2);
    VERIFY( !ok && a.load() == 7 && expected == 7 );
    expected = a.load();
    expected.i += 1;
    ok = a.compare_exchange_strong(expected, 2, mo);
    VERIFY( !ok && a.load() == 7 && expected == 7 );
  }

  VERIFY( value == 7 );
}

void
test02()
{
  X i;
  std::atomic_ref<X> a0(i);
  std::atomic_ref<X> a1(i);
  std::atomic_ref<X> a2(a0);
  a0 = 42;
  VERIFY( a1.load() == 42 );
  VERIFY( a2.load() == 42 );
}

int
main()
{
  test01();
  test02();
}

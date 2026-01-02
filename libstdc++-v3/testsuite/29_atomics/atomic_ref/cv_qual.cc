// Copyright (C) 2019-2026 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

struct X
{
  X() = default;
  X(int i) : i(i) { }
  int i;

  friend bool
  operator==(X, X) = default;
};

template<typename V>
void
test01(V v0, V v1)
{
  V value;

  if constexpr (std::atomic_ref<V>::is_always_lock_free)
  {
    std::atomic_ref<volatile V> a(value);
    VERIFY( a.is_lock_free() );

    a = v0;
    VERIFY( V(a) == v0 );
    VERIFY( a.load() == v0 );

    a.store(v1);
    VERIFY( a.load() == v1 );

    V last = a.exchange(v0);
    VERIFY( a.load() == v0 );
    VERIFY( last == v1 );

    V expected = a.load();
    while (!a.compare_exchange_weak(expected, v1))
    { /* weak form can fail spuriously */ }
    VERIFY( a.load() == v1 );
    VERIFY( expected == v0 );

    bool ok;
    ok = a.compare_exchange_strong(expected, v0);
    VERIFY( !ok && a.load() == v1 && expected == v1 );

    ok = a.compare_exchange_strong(expected, v0);
    VERIFY( ok && a.load() == v0 && expected == v1 );

    std::atomic_ref<const volatile V> cva(value);
    VERIFY( cva.is_lock_free() );
    VERIFY( V(cva) == v0 );
    VERIFY( cva.load() == v0 );
  }

  value = v0;
  std::atomic_ref<const V> ca(value);
  bool lf = ca.is_lock_free();
  if constexpr (std::atomic_ref<V>::is_always_lock_free)
    VERIFY( lf );
  VERIFY( V(ca) == v0 );
  VERIFY( ca.load() == v0 );
}

int
main()
{
  int x;
  test01<bool>(false, true);
  test01<int>(1, 2);
  test01<float>(1.2, 3.4);
  test01<int*>(&x, &x+1);
  test01<X>(12, 13);
}

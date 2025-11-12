// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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
#include <type_traits>

template<typename T>
using volatile_
 = std::conditional_t<std::atomic_ref<T>::is_always_lock_free, volatile T, T>;

void
test01()
{
  bool value;
  {
    const auto mo = std::memory_order_relaxed;
    std::atomic_ref<bool> a(value);
    bool ok = a.is_lock_free();
    if constexpr (std::atomic_ref<bool>::is_always_lock_free)
      VERIFY( ok );
    a = false;
    VERIFY( !a.load() );
    VERIFY( !a.load(mo) );
    a.store(true);
    VERIFY( a.load() );
    auto v = a.exchange(false);
    VERIFY( !a.load() );
    VERIFY( v );
    v = a.exchange(true, mo);
    VERIFY( a.load() );
    VERIFY( !v );

    auto expected = a.load();
    while (!a.compare_exchange_weak(expected, false, mo, mo))
    { /* weak form can fail spuriously */ }
    VERIFY( !a.load() );
    VERIFY( expected );

    ok = a.compare_exchange_strong(expected, true);
    VERIFY( !ok && !a.load() && !expected );

    ok = a.compare_exchange_strong(expected, true);
    VERIFY( ok && a.load() && !expected );
  }
}

void
test02()
{
  bool b = false;
  std::atomic_ref<bool> a0(b);
  std::atomic_ref<bool> a1(b);
  std::atomic_ref<const bool> a1c(b);
  std::atomic_ref<volatile_<bool>> a1v(b);
  std::atomic_ref<volatile_<const bool>> a1cv(b);
  std::atomic_ref<bool> a2(a0);
  b = true;
  VERIFY( a1.load() );
  VERIFY( a1c.load() );
  VERIFY( a1v.load() );
  VERIFY( a1cv.load() );
  VERIFY( a2.load() );
}

int
main()
{
  test01();
  test02();
}

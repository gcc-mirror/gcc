// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-gthreads "" }
// { dg-require-effective-target hosted }

#include <memory>

int main()
{
  using test_type = std::shared_ptr<int>;
  bool test __attribute__ ((unused)) = false;
  constexpr auto mo = std::memory_order_seq_cst;
  const test_type p;
  test = std::atomic_is_lock_free(&p);
  test_type p2 = std::atomic_load(&p);
  test_type p3 = std::atomic_load_explicit(&p, mo);
  std::atomic_store(&p2, p);
  std::atomic_store_explicit(&p2, p, mo);
  test_type p4 = std::atomic_exchange(&p2, p);
  p4 = std::atomic_exchange_explicit(&p2, p, mo);
  test = std::atomic_compare_exchange_weak(&p2, &p3, p);
  test = std::atomic_compare_exchange_strong(&p2, &p3, p);
  test = std::atomic_compare_exchange_weak_explicit(&p2, &p3, p, mo, mo);
  test = std::atomic_compare_exchange_strong_explicit(&p2, &p3, p, mo, mo);
}

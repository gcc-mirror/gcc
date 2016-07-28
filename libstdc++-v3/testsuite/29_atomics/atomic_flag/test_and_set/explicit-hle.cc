// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-std=gnu++11 -g0 -fno-exceptions -fno-asynchronous-unwind-tables" }
// { dg-additional-options "-march=i486" { target ia32 } }
// { dg-final { scan-assembler-times "xacquire\|\.byte\[^\n\r]*0xf2" 14 } }
// { dg-final { scan-assembler-times "xrelease\|\.byte\[^\n\r]*0xf3" 14 } }

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

#include <atomic>

#define ACQ memory_order_acquire | __memory_order_hle_acquire
#define REL memory_order_release | __memory_order_hle_release

int main()
{
  unsigned zero, one;
  using namespace std;
  atomic_flag af = ATOMIC_FLAG_INIT;

  if (!af.test_and_set(ACQ))
    af.clear(REL);

  atomic_uint au = ATOMIC_VAR_INIT(0);

  if (au.exchange(1, ACQ))
    au.store(0, REL);

  if (au.exchange(1, ACQ))
    au.exchange(0, REL);

  zero = 0;
  one = 1;
  if (au.compare_exchange_weak(zero, 1, ACQ, memory_order_consume))
    au.compare_exchange_weak(one, 0, REL, memory_order_consume);

  zero = 0;
  one = 1;
  if (au.compare_exchange_strong(zero, 1, ACQ, memory_order_consume))
    au.compare_exchange_strong(one, 0, REL, memory_order_consume);

  if (!au.fetch_add(1, ACQ))
    au.fetch_add(-1, REL);

  if (!au.fetch_sub(1, ACQ))
    au.fetch_sub(-1, REL);

#if 0 /* broken in underlying target */
  if (!au.fetch_and(1, ACQ))
    au.fetch_and(-1, REL);

  if (!au.fetch_or(1, ACQ))
    au.fetch_or(-1, REL);

  if (!au.fetch_xor(1, ACQ))
    au.fetch_xor(-1, REL);

  if (!au.fetch_nand(1, ACQ))
    au.fetch_nand(-1, REL);
#endif

  volatile atomic_flag vaf = ATOMIC_FLAG_INIT;

  if (!vaf.test_and_set(ACQ))
    vaf.clear(REL);

  volatile atomic_uint vau = ATOMIC_VAR_INIT(0);

  if (!vau.exchange(1, ACQ))
    vau.store(0, REL);

  if (!vau.exchange(1, ACQ))
    vau.exchange(0, REL);

  zero = 0;
  one = 1;
  if (vau.compare_exchange_weak(zero, 1, ACQ, memory_order_consume))
    vau.compare_exchange_weak(one, 0, REL, memory_order_consume);

  zero = 0;
  one = 1;
  if (vau.compare_exchange_strong(zero, 1, ACQ, memory_order_consume))
    vau.compare_exchange_strong(one, 0, REL, memory_order_consume);

  if (!vau.fetch_add(1, ACQ))
    vau.fetch_add(-1, REL);

  if (!vau.fetch_sub(1, ACQ))
    vau.fetch_sub(-1, REL);

#if 0 /* broken in underlying target */

  if (!vau.fetch_and(1, ACQ))
    vau.fetch_and(-1, REL);

  if (!vau.fetch_or(1, ACQ))
    vau.fetch_or(-1, REL);

  if (!vau.fetch_xor(1, ACQ))
    vau.fetch_xor(-1, REL);

  if (!vau.fetch_nand(1, ACQ))
    vau.fetch_nand(-1, REL);
#endif

  return 0;
}

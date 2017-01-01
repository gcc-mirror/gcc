/* Copyright (C) 2008-2017 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Provide target-specific access to the futex system call.  */

#ifndef SYS_futex
#define SYS_futex               394
#endif

static inline long
sys_futex0 (std::atomic<int> *addr, int op, int val)
{
  register long sc_0 __asm__("$0");
  register long sc_16 __asm__("$16");
  register long sc_17 __asm__("$17");
  register long sc_18 __asm__("$18");
  register long sc_19 __asm__("$19");
  long res;

  sc_0 = SYS_futex;
  sc_16 = (long) addr;
  sc_17 = op;
  sc_18 = val;
  sc_19 = 0;
  __asm volatile ("callsys"
		  : "=r" (sc_0), "=r"(sc_19)
		  : "0"(sc_0), "r" (sc_16), "r"(sc_17), "r"(sc_18), "1"(sc_19)
		  : "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8",
		    "$22", "$23", "$24", "$25", "$27", "$28", "memory");

  res = sc_0;
  if (__builtin_expect (sc_19, 0))
    res = -res;
  return res;
}

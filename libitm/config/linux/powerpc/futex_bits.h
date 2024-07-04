/* Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

#include <sys/syscall.h>

static inline long
sys_futex0 (std::atomic<int> *addr, int op, int val)
{
  register long int r0  __asm__ ("r0");
  register long int r3  __asm__ ("r3");
  register long int r4  __asm__ ("r4");
  register long int r5  __asm__ ("r5");
  register long int r6  __asm__ ("r6");

  r0 = SYS_futex;
  r3 = (long) addr;
  r4 = op;
  r5 = val;
  r6 = 0;

  /* ??? The powerpc64 sysdep.h file clobbers ctr; the powerpc32 sysdep.h
     doesn't.  It doesn't much matter for us.  In the interest of unity,
     go ahead and clobber it always.  */

  __asm volatile ("sc; mfcr %0"
		  : "=r"(r0), "=r"(r3), "=r"(r4), "=r"(r5), "=r"(r6)
		  : "r"(r0), "r"(r3), "r"(r4), "r"(r5), "r"(r6)
		  : "r7", "r8", "r9", "r10", "r11", "r12",
		    "cr0", "ctr", "memory");
  if (__builtin_expect (r0 & (1 << 28), 0))
    return r3;
  return 0;
}

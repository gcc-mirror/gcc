/* Copyright (C) 2005, 2008, 2009 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU OpenMP Library (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
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

#include <sys/syscall.h>



static inline long
sys_futex0(int *addr, long op, int val)
{
  register long out0 asm ("out0") = (long) addr;
  register long out1 asm ("out1") = op;
  register long out2 asm ("out2") = val;
  register long out3 asm ("out3") = 0;
  register long r8 asm ("r8");
  register long r10 asm ("r10");
  register long r15 asm ("r15") = SYS_futex;

  __asm __volatile ("break 0x100000"
	: "=r"(r15), "=r"(out0), "=r"(out1), "=r"(out2), "=r"(out3),
	  "=r"(r8), "=r"(r10)
	: "r"(r15), "r"(out0), "r"(out1), "r"(out2), "r"(out3)
	: "memory", "out4", "out5", "out6", "out7",
	  /* Non-stacked integer registers, minus r8, r10, r15.  */
	  "r2", "r3", "r9", "r11", "r12", "r13", "r14", "r16", "r17", "r18",
	  "r19", "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27",
	  "r28", "r29", "r30", "r31",
	  /* Predicate registers.  */
	  "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14", "p15",
	  /* Non-rotating fp registers.  */
	  "f6", "f7", "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",
	  /* Branch registers.  */
	  "b6");
  return r8 & r10;
}

static inline void
futex_wait (int *addr, int val)
{
  long err = sys_futex0 (addr, gomp_futex_wait, val);
  if (__builtin_expect (err == ENOSYS, 0))
    {
      gomp_futex_wait &= ~FUTEX_PRIVATE_FLAG;
      gomp_futex_wake &= ~FUTEX_PRIVATE_FLAG;
      sys_futex0 (addr, gomp_futex_wait, val);
    }
}

static inline void
futex_wake (int *addr, int count)
{
  long err = sys_futex0 (addr, gomp_futex_wake, count);
  if (__builtin_expect (err == ENOSYS, 0))
    {
      gomp_futex_wait &= ~FUTEX_PRIVATE_FLAG;
      gomp_futex_wake &= ~FUTEX_PRIVATE_FLAG;
      sys_futex0 (addr, gomp_futex_wake, count);
    }
}

static inline void
cpu_relax (void)
{
  __asm volatile ("hint @pause" : : : "memory");
}

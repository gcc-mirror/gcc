/* Copyright (C) 2005-2016 Free Software Foundation, Inc.
   Contributed by Ilie Garbacea <ilie@mips.com>, Chao-ying Fu <fu@mips.com>.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

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

#if !defined (SYS_futex)
#define SYS_futex __NR_futex
#endif

#define FUTEX_WAIT 0
#define FUTEX_WAKE 1

#ifdef __mips16
static void __attribute__((noinline,nomips16))
#else
static inline void
#endif
sys_futex0 (int *addr, int op, int val)
{
  register unsigned long __v0 asm("$2");
  register unsigned long __a0 asm("$4") = (unsigned long) addr;
  register unsigned long __a1 asm("$5") = (unsigned long) op;
  register unsigned long __a2 asm("$6") = (unsigned long) val;
  register unsigned long __a3 asm("$7") = 0;

  __asm volatile ("li $2, %6\n\t"
		  "syscall"
		  /* returns $a3 (errno), $v0 (return value) */
		  : "=r" (__v0), "=r" (__a3)
		  /* arguments in a0-a3, and syscall number */
		  : "r" (__a0), "r" (__a1), "r" (__a2), "r" (__a3),
                    "IK" (SYS_futex)
		  /* clobbers at, v1, t0-t9, memory */
		  : "$1", "$3", "$8", "$9", "$10", "$11", "$12", "$13", "$14",
		    "$15", "$24", "$25", "memory");
}

static inline void
futex_wait (int *addr, int val)
{
  sys_futex0 (addr, FUTEX_WAIT, val);
}

static inline void
futex_wake (int *addr, int count)
{
  sys_futex0 (addr, FUTEX_WAKE, count);
}

static inline void
cpu_relax (void)
{
  __asm volatile ("" : : : "memory");
}

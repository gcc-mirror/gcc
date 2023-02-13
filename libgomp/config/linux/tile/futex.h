/* Copyright (C) 2011-2023 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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
#include <linux/futex.h>

static inline void
sys_futex0 (int *addr, int op, int val)
{
  long _sys_result;
  long _clobber_r2, _clobber_r3, _clobber_r4, _clobber_r5, _clobber_r10;
  int err;

  __asm__ __volatile__ (
    "swint1"
    : "=R00" (_sys_result), "=R01" (err), "=R02" (_clobber_r2),
      "=R03" (_clobber_r3), "=R04" (_clobber_r4), "=R05" (_clobber_r5),
      "=R10" (_clobber_r10)
    : "R10" (SYS_futex), "R00" (addr), "R01" (op), "R02" (val),
      "R03" (0)
    :  "r6",  "r7",
       "r8",  "r9",        "r11", "r12", "r13", "r14", "r15",
      "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
      "r24", "r25", "r26", "r27", "r28", "r29", "memory");
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

static inline void
atomic_write_barrier (void)
{
  __sync_synchronize ();
}

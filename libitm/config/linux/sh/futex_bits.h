/* Copyright (C) 2011-2021 Free Software Foundation, Inc.

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

#include <sys/syscall.h>

/* 4 instruction cycles not accessing cache and TLB are needed after
   trapa instruction to avoid an SH-4 silicon bug.  */

#define SYSCALL_WITH_INST_PAD "\
       trapa #0x14; or r0,r0; or r0,r0; or r0,r0; or r0,r0; or r0,r0"

static inline long
sys_futex0 (std::atomic<int> *addr, int op, int val)
{
  int __status;
  register long __r3 asm ("r3") = SYS_futex;
  register long __r4 asm ("r4") = (long) addr;
  register long __r5 asm ("r5") = op;
  register long __r6 asm ("r6") = val;
  register long __r7 asm ("r7") = 0;

  __asm __volatile (SYSCALL_WITH_INST_PAD
		    : "=z" (__status)
		    : "r" (__r3), "r" (__r4), "r" (__r5),
		    "r" (__r6), "r" (__r7)
		    : "memory", "t");
  return __status;
}

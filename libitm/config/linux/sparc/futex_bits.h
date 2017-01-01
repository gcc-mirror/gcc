/* Copyright (C) 2012-2017 Free Software Foundation, Inc.

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
  register long int g1 __asm__ ("g1");
  register long int o0 __asm__ ("o0");
  register long int o1 __asm__ ("o1");
  register long int o2 __asm__ ("o2");
  register long int o3 __asm__ ("o3");
  long res;

  g1 = SYS_futex;
  o0 = (long) addr;
  o1 = op;
  o2 = val;
  o3 = 0;

#ifdef __arch64__
  __asm volatile ("ta 0x6d"
#else
  __asm volatile ("ta 0x10"
#endif
		  : "=r"(g1), "=r"(o0)
		  : "0"(g1), "1"(o0), "r"(o1), "r"(o2), "r"(o3)
		  : "g2", "g3", "g4", "g5", "g6",
		    "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
		    "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",
		    "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",
		    "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",
		    "f32", "f34", "f36", "f38", "f40", "f42", "f44", "f46",
		    "f48", "f50", "f52", "f54", "f56", "f58", "f60", "f62",
		    "cc", "memory");

  res = o0;
  if (__builtin_expect ((unsigned long) res >= -515UL, 0))
    res =- res;
  return res;
}

/* Copyright (C) 2005-2013 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

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
sys_futex0 (int *addr, int op, int val)
{
  register long int g1  __asm__ ("g1");
  register long int o0  __asm__ ("o0");
  register long int o1  __asm__ ("o1");
  register long int o2  __asm__ ("o2");
  register long int o3  __asm__ ("o3");

  g1 = SYS_futex;
  o0 = (long) addr;
  o1 = op;
  o2 = val;
  o3 = 0;

#ifdef __arch64__
# define SYSCALL_STRING "ta\t0x6d; bcs,a,pt %%xcc, 1f; sub %%g0, %%o0, %%o0; 1:"
#else
# define SYSCALL_STRING "ta\t0x10; bcs,a 1f; sub %%g0, %%o0, %%o0; 1:"
#endif

  __asm volatile (SYSCALL_STRING
		  : "=r" (g1), "=r" (o0)
		  : "0" (g1), "1" (o0), "r" (o1), "r" (o2), "r" (o3)
		  : "g2", "g3", "g4", "g5", "g6",
		    "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",
		    "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",
		    "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",
		    "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",
#ifdef __arch64__
		    "f32", "f34", "f36", "f38", "f40", "f42", "f44", "f46",
		    "f48", "f50", "f52", "f54", "f56", "f58", "f60", "f62",
#endif
		    "cc", "memory");
  return o0;
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
  __asm volatile ("rd %%ccr, %%g0" : : : "memory");
}

/* Copyright (C) 2005-2025 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

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

static inline long
sys_futex0 (int *addr, int op, int val)
{
  register long int gpr2  __asm__ ("2");
  register long int gpr3  __asm__ ("3");
  register long int gpr4  __asm__ ("4");
  register long int gpr5  __asm__ ("5");

  gpr2 = (long) addr;
  gpr3 = op;
  gpr4 = val;
  gpr5 = 0;

  __asm volatile ("svc %b1"
		  : "=d" (gpr2)
		  : "i" (SYS_futex),
		    "0" (gpr2), "d" (gpr3), "d" (gpr4), "d" (gpr5)
		  : "memory");
  return gpr2;
}

static inline void
futex_wait (int *addr, int val)
{
  long err = sys_futex0 (addr, gomp_futex_wait, val);
  if (__builtin_expect (err == -ENOSYS, 0))
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
  if (__builtin_expect (err == -ENOSYS, 0))
    {
      gomp_futex_wait &= ~FUTEX_PRIVATE_FLAG;
      gomp_futex_wake &= ~FUTEX_PRIVATE_FLAG;
      sys_futex0 (addr, gomp_futex_wake, count);
    }
}

static inline void
cpu_relax (void)
{
  __asm volatile ("" : : : "memory");
}

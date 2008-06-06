/* Copyright (C) 2005, 2008 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

   This file is part of the GNU OpenMP Library (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 2.1 of the License, or
   (at your option) any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
   more details.

   You should have received a copy of the GNU Lesser General Public License
   along with libgomp; see the file COPYING.LIB.  If not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.  */

/* As a special exception, if you link this library with other files, some
   of which are compiled with GCC, to produce an executable, this library
   does not by itself cause the resulting executable to be covered by the
   GNU General Public License.  This exception does not however invalidate
   any other reasons why the executable file might be covered by the GNU
   General Public License.  */

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

static inline void
atomic_write_barrier (void)
{
  __sync_synchronize ();
}

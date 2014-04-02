/* Copyright (C) 2010, 2011 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

/* The include file hierachy above us (wait.h) has pushed visibility
   hidden, this will be applied to prototypes with headers we include
   with the effect that we cannot link against an external function
   (syscall). The solution here is to push default visibility, include
   our required headers then reinstante the original visibility.  */

#pragma GCC visibility push(default)

#define _GNU_SOURCE 
#include <unistd.h>
#include <sys/syscall.h>

#pragma GCC visibility pop

static inline void
futex_wait (int *addr, int val)
{
  int err = syscall (SYS_futex, addr, gomp_futex_wait, val, NULL);
  if (__builtin_expect (err < 0 && errno == ENOSYS, 0))
    {
      gomp_futex_wait &= ~FUTEX_PRIVATE_FLAG;
      gomp_futex_wake &= ~FUTEX_PRIVATE_FLAG;
      syscall (SYS_futex, addr, gomp_futex_wait, val, NULL);
    }
}

static inline void
futex_wake (int *addr, int count)
{
  int err = syscall (SYS_futex, addr, gomp_futex_wake, count);
  if (__builtin_expect (err < 0 && errno == ENOSYS, 0))
    {
      gomp_futex_wait &= ~FUTEX_PRIVATE_FLAG;
      gomp_futex_wake &= ~FUTEX_PRIVATE_FLAG;
      syscall (SYS_futex, addr, gomp_futex_wake, count);
    }
}

static inline void
cpu_relax (void)
{
  __asm volatile ("" : : : "memory");
}

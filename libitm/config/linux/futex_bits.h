/* Copyright (C) 2011-2014 Free Software Foundation, Inc.
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

/* Provide target-independant access to the futex system call.  */

/* Note for ARM:
   There are two styles of syscall, and in the eabi style the syscall
   number goes into the thumb frame pointer.  We need to either write
   this in pure assembler or just defer entirely to libc.  */

#include <unistd.h>
#include <sys/syscall.h>
#include <errno.h>

static inline long
sys_futex0 (std::atomic<int> *addr, long op, long val)
{
  long res = syscall (SYS_futex, (int*) addr, op, val, 0);
  if (__builtin_expect (res == -1, 0))
    return -errno;
  return res;
}

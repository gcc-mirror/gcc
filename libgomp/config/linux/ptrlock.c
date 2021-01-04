/* Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

/* This is a Linux specific implementation of a mutex synchronization
   mechanism for libgomp.  This type is private to the library.  This
   implementation uses atomic instructions and the futex syscall.  */

#include <endian.h>
#include <limits.h>
#include "wait.h"

void *
gomp_ptrlock_get_slow (gomp_ptrlock_t *ptrlock)
{
  int *intptr;
  uintptr_t oldval = 1;

  __atomic_compare_exchange_n (ptrlock, &oldval, 2, false,
			       MEMMODEL_RELAXED, MEMMODEL_RELAXED);

  /* futex works on ints, not pointers.
     But a valid work share pointer will be at least
     8 byte aligned, so it is safe to assume the low
     32-bits of the pointer won't contain values 1 or 2.  */
  __asm volatile ("" : "=r" (intptr) : "0" (ptrlock));
#if __BYTE_ORDER == __BIG_ENDIAN
  if (sizeof (*ptrlock) > sizeof (int))
    intptr += (sizeof (*ptrlock) / sizeof (int)) - 1;
#endif
  do
    do_wait (intptr, 2);
  while (__atomic_load_n (intptr, MEMMODEL_RELAXED) == 2);
  __asm volatile ("" : : : "memory");
  return (void *) __atomic_load_n (ptrlock, MEMMODEL_ACQUIRE);
}

void
gomp_ptrlock_set_slow (gomp_ptrlock_t *ptrlock)
{
  int *intptr;

  __asm volatile ("" : "=r" (intptr) : "0" (ptrlock));
#if __BYTE_ORDER == __BIG_ENDIAN
  if (sizeof (*ptrlock) > sizeof (int))
    intptr += (sizeof (*ptrlock) / sizeof (int)) - 1;
#endif
  futex_wake (intptr, INT_MAX);
}

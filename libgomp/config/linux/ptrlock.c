/* Copyright (C) 2008 Free Software Foundation, Inc.
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
  __sync_bool_compare_and_swap (ptrlock, 1, 2);

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
  while (*intptr == 2);
  __asm volatile ("" : : : "memory");
  return *ptrlock;
}

void
gomp_ptrlock_set_slow (gomp_ptrlock_t *ptrlock, void *ptr)
{
  int *intptr;

  *ptrlock = ptr;
  __asm volatile ("" : "=r" (intptr) : "0" (ptrlock));
#if __BYTE_ORDER == __BIG_ENDIAN
  if (sizeof (*ptrlock) > sizeof (int))
    intptr += (sizeof (*ptrlock) / sizeof (int)) - 1;
#endif
  futex_wake (intptr, INT_MAX);
}

/* Low-level functions for atomic operations.  AIX version.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef _BITS_ATOMICITY_H
#define _BITS_ATOMICITY_H       1

/* We cannot use the cpu/powerpc/bits/atomicity.h inline assembly
   definitions for these operations since they depend on operations
   that are not available on the original POWER architecture.  AIX
   still runs on the POWER architecture, so it would be incorrect to
   assume the existence of these instructions.  */

/* This should match the type pointed to by atomic_p in
   <sys/atomic_op.h>.  */
typedef int _Atomic_word;

#include <sys/atomic_op.h>

static inline int
__attribute__ ((unused))
__exchange_and_add (atomic_p __mem, int __val)
{
  return fetch_and_add (__mem, __val);
}

static inline void
__attribute__ ((unused))
__atomic_add (atomic_p __mem, int __val)
{
  (void) fetch_and_add (__mem, __val);
}

#endif /* atomicity.h */

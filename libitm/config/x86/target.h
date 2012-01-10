/* Copyright (C) 2008, 2009, 2011 Free Software Foundation, Inc.
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

namespace GTM HIDDEN {

#ifdef __x86_64__
/* ??? This doesn't work for Win64.  */
typedef struct gtm_jmpbuf
{
  void *cfa;
  unsigned long rip;
  unsigned long rbx;
  unsigned long rbp;
  unsigned long r12;
  unsigned long r13;
  unsigned long r14;
  unsigned long r15;
} gtm_jmpbuf;
#else
typedef struct gtm_jmpbuf
{
  void *cfa;
  unsigned long ebx;
  unsigned long esi;
  unsigned long edi;
  unsigned long ebp;
  unsigned long eip;
} gtm_jmpbuf;
#endif

/* x86 doesn't require strict alignment for the basic types.  */
#define STRICT_ALIGNMENT 0

/* x86 uses a fixed page size of 4K.  */
#define PAGE_SIZE       4096
#define FIXED_PAGE_SIZE 1

/* The size of one line in hardware caches (in bytes). */
#define HW_CACHELINE_SIZE 64


static inline void
cpu_relax (void)
{
  __asm volatile ("rep; nop" : : : "memory");
}

} // namespace GTM

// We'll be using some of the cpu builtins, and their associated types.
#ifndef __cplusplus
/* ??? It's broken for C++. */
#include <x86intrin.h>
#else
# ifdef __SSE2__
#  include <emmintrin.h>
# elif defined(__SSE__)
#  include <xmmintrin.h>
# endif
# ifdef __AVX__
#  include <immintrin.h>
# endif
#endif

/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

typedef int v128 __attribute__((vector_size(16), may_alias, aligned(16)));
typedef struct gtm_jmpbuf
{
#if defined(__ALTIVEC__) || defined(__VSX__)
  v128 vr[12];			/* vr20-vr31 */
  unsigned long long vscr;	/* long long for padding only */
#endif
#ifndef _SOFT_FLOAT
  double fr[18];		/* f14-f31 */
  double fpscr;
#endif
  unsigned long gr[18];		/* r14-r31 */
  void *cfa;
  unsigned long pc;
  unsigned long toc;		/* r2 on aix, r13 on darwin */
  unsigned long cr;
} gtm_jmpbuf;

/* The size of one line in hardware caches (in bytes). */
#if defined (__powerpc64__) || defined (__ppc64__)
# define HW_CACHELINE_SIZE 128
#else
# define HW_CACHELINE_SIZE 32
#endif

static inline void
cpu_relax (void)
{
  __asm volatile ("" : : : "memory");
}

} // namespace GTM

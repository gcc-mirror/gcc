/* Copyright (C) 2011-2014 Free Software Foundation, Inc.

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

typedef struct gtm_jmpbuf
{
  unsigned long s[7];	/* r8-r14 */
  void *cfa;
  unsigned long pc;
  unsigned long gbr;
#ifdef __SH_FPU_ANY__
  unsigned long fpscr;
  unsigned long f[4];	/* fr12-fr15 */
#endif
} gtm_jmpbuf;

/* SH generally uses a fixed page size of 4K.  */
#define PAGE_SIZE	4096
#define FIXED_PAGE_SIZE	1

/* ??? The size of one line in hardware caches (in bytes). */
#define HW_CACHELINE_SIZE 32

static inline void
cpu_relax (void)
{
  __asm volatile ("" : : : "memory");
}

} // namespace GTM

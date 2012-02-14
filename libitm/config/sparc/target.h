/* Copyright (C) 2012 Free Software Foundation, Inc.

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
  void *cfa;
  unsigned long pc;
} gtm_jmpbuf;

/* UltraSPARC processors generally use a fixed page size of 8K.  */
#define PAGE_SIZE	8192
#define FIXED_PAGE_SIZE	1

/* The size of one line in hardware caches (in bytes).  We use the primary
   cache line size documented for the UltraSPARC T1/T2.  */
#define HW_CACHELINE_SIZE 16

static inline void
cpu_relax (void)
{
  __asm volatile ("rd %%ccr, %%g0" : : : "memory");
}

} // namespace GTM

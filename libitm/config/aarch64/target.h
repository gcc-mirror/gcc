/* Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

typedef struct gtm_jmpbuf
{
  unsigned long long fp;	/* x29 */
  unsigned long long pc;	/* x30 */
  unsigned long long gr[10];	/* x19-x28 */
  unsigned long long vr[8];	/* d8-d15 */
  void *cfa;
} gtm_jmpbuf;

/* ??? The size of one line in hardware caches (in bytes). */
#define HW_CACHELINE_SIZE 128

static inline void
cpu_relax (void)
{
  __asm volatile ("" : : : "memory");
}

} // namespace GTM

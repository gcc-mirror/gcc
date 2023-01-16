/* Copyright (C) 2011-2023 Free Software Foundation, Inc.
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
  unsigned long long vfp[8];	/* d8-d15 */
  unsigned long long iwmmxt[6];	/* cr10-cr15 */
  unsigned long gr[8];		/* r4-r11 */
  void *cfa;
  unsigned long pc;
} gtm_jmpbuf;

/* ??? The size of one line in hardware caches (in bytes). */
#define HW_CACHELINE_SIZE 64

static inline void
cpu_relax (void)
{
  /* ??? The kernel uses the condition
	#if __LINUX_ARM_ARCH__ == 6 || defined(CONFIG_ARM_ERRATA_754327)
     Given that we're actually just waiting, it doesn't seem like it 
     hurts to simply use a full barrier all the time.  */
  __sync_synchronize ();
}

} // namespace GTM

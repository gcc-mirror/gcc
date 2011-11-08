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

#ifndef LIBITM_X86_TLS_H
#define LIBITM_X86_TLS_H 1

#if defined(__GLIBC_PREREQ) && __GLIBC_PREREQ(2, 10)
/* Use slots in the TCB head rather than __thread lookups.
   GLIBC has reserved words 10 through 13 for TM.  */
#define HAVE_ARCH_GTM_THREAD 1
#define HAVE_ARCH_GTM_THREAD_DISP 1
#endif

#include "config/generic/tls.h"

#if defined(__GLIBC_PREREQ) && __GLIBC_PREREQ(2, 10)
namespace GTM HIDDEN {

#ifdef __x86_64__
#ifdef __LP64__
# define SEG_READ(OFS)		"movq\t%%fs:(" #OFS "*8),%0"
# define SEG_WRITE(OFS)		"movq\t%0,%%fs:(" #OFS "*8)"
# define SEG_DECODE_READ(OFS)	SEG_READ(OFS) "\n\t" \
				"rorq\t$17,%0\n\t" \
				"xorq\t%%fs:48,%0"
# define SEG_ENCODE_WRITE(OFS)	"xorq\t%%fs:48,%0\n\t" \
				"rolq\t$17,%0\n\t" \
				SEG_WRITE(OFS)
#else
// For X32.
# define SEG_READ(OFS)          "movl\t%%fs:(" #OFS "*4),%0"
# define SEG_WRITE(OFS)         "movl\t%0,%%fs:(" #OFS "*4)"
# define SEG_DECODE_READ(OFS)   SEG_READ(OFS) "\n\t" \
				"rorl\t$9,%0\n\t" \
				"xorl\t%%fs:24,%0"
# define SEG_ENCODE_WRITE(OFS)  "xorl\t%%fs:24,%0\n\t" \
				"roll\t$9,%0\n\t" \
				SEG_WRITE(OFS)
#endif
#else
# define SEG_READ(OFS)  "movl\t%%gs:(" #OFS "*4),%0"
# define SEG_WRITE(OFS) "movl\t%0,%%gs:(" #OFS "*4)"
# define SEG_DECODE_READ(OFS)	SEG_READ(OFS) "\n\t" \
				"rorl\t$9,%0\n\t" \
				"xorl\t%%gs:24,%0"
# define SEG_ENCODE_WRITE(OFS)	"xorl\t%%gs:24,%0\n\t" \
				"roll\t$9,%0\n\t" \
				SEG_WRITE(OFS)
#endif

static inline struct gtm_thread *gtm_thr(void)
{
  struct gtm_thread *r;
  asm volatile (SEG_READ(10) : "=r"(r));
  return r;
}

static inline void set_gtm_thr(struct gtm_thread *x)
{
  asm volatile (SEG_WRITE(10) : : "r"(x));
}

static inline struct abi_dispatch *abi_disp(void)
{
  struct abi_dispatch *r;
  asm volatile (SEG_DECODE_READ(11) : "=r"(r));
  return r;
}

static inline void set_abi_disp(struct abi_dispatch *x)
{
  void *scratch;
  asm volatile (SEG_ENCODE_WRITE(11) : "=r"(scratch) : "0"(x));
}

#undef SEG_READ
#undef SEG_WRITE
#undef SEG_DECODE_READ
#undef SEG_ENCODE_WRITE

} // namespace GTM
#endif /* >= GLIBC 2.10 */

#endif // LIBITM_X86_TLS_H

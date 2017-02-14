/* Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
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

#include <libatomic_i.h>
#include <arm-config.h>


/* When using STREX to implement sub-word exchange, we can do much better
   than the compiler by using the APSR.GE and APSR.C flags.  */

#if !DONE && __ARM_FEATURE_SIMD32 && HAVE_STREX && !HAVE_STREXBH && N == 2
UTYPE
SIZE(libat_exchange) (UTYPE *mptr, UTYPE newval, int smodel)
{
  UWORD t1, t2;
  UTYPE oldval;

  __atomic_thread_fence (__ATOMIC_SEQ_CST);

  /* In the N=2 case, there are only two cases for MPTR: mptr % 4 == {0,2}.
     Rather than computing a variable shift for this, we can store the one
     bit of misalignment in the carry flag, and use conditional constant
     shifts instead.  This saves a register.  */
#ifdef __ARMEB__
# define HI	"cc"				/* iff value is in high half */
# define LO	"cs"				/* iff value is in low half */
#else
# define HI	"cs"
# define LO	"cc"
#endif

  asm volatile (
		"lsrs	%[t2],%[ptr],#2\n"	/* carry = mptr & 2 */
	"	bic	%[ptr],%[ptr],#3\n"	/* align mptr */
	"	itt	"HI"\n"
	"	lsl"HI"	%[t1],%[t1],#16\n"	/* shift mask into place */
	"	lsl"HI"	%[new],%[new],#16\n"	/* shift newval into place */
	"	uadd16	%[t1],%[t1],%[t1]\n"	/* copy mask into APSR.GE */
	"0:	ldrex	%[t2],[%[ptr]]\n"
	"	ite	"LO"\n"
	"	uxth"LO" %[old],%[t2]\n"	/* return old value */
	"	uxth"HI" %[old],%[t2], ror #16\n"
	"	sel	%[t1],%[new],%[t2]\n"	/* merge newval */
	"	strex	%[t2],%[t1],[%[ptr]]\n"
	"	tst	%[t2],%[t2]\n"		/* dont clobber carry */
	"	bne	0b"
	: [old] "=&r"(oldval), [t1] "=&r"(t1), [t2] "=&r"(t2),
	  [ptr] "+r"(mptr), [new] "+r"(newval)
	: "1"(0xffff)
	: "memory");

  __atomic_thread_fence (__ATOMIC_SEQ_CST);

  return oldval;
}

#define DONE 1
#endif /* !HAVE_STREXBH && N == 2 */


#if !DONE && __ARM_FEATURE_SIMD32 && HAVE_STREX && !HAVE_STREXBH && N == 1
UTYPE
SIZE(libat_exchange) (UTYPE *mptr, UTYPE newval, int smodel)
{
  UWORD *wptr, woldval, wnewval, shift, mask, t1, t2;

  __atomic_thread_fence (__ATOMIC_SEQ_CST);

  wptr = (UWORD *)((uintptr_t)mptr & -WORDSIZE);
  shift = (((uintptr_t)mptr % WORDSIZE) * CHAR_BIT) ^ INVERT_MASK_1;
  mask = MASK_1 << shift;
  wnewval = newval << shift;

  asm volatile (
		"uadd8	%[t1],%[t1],%[t1]\n"	/* move mask to APSR.GE */
	"0:	ldrex	%[old],[%[wptr]]\n"
	"	sel	%[t1],%[new],%[old]\n"	/* merge newval */
	"	strex	%[t2],%[t1],[%[wptr]]\n"
	"	cmp	%[t2],#0\n"
	"	bne	0b"
	: [old] "=&r"(woldval), [t1] "=&r"(t1), [t2] "=&r"(t2)
	: [new] "r"(wnewval), [wptr] "r"(wptr), "1"(mask)
	: "memory");

  __atomic_thread_fence (__ATOMIC_SEQ_CST);

  return woldval >> shift;
}

#define DONE 1
#endif /* !HAVE_STREXBH && N == 1 */

#include "../../exch_n.c"

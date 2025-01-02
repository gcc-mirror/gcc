/*
 * Copyright (C) 1997-2025 Free Software Foundation, Inc.
 *
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 *
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 *
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 */


#include "sfp-machine.h"

#define HUGE_VAL (__builtin_huge_val ())

/* Please see section 10,
   page 10-5 "Delayed Trapping" in the PA-RISC 2.0 Architecture manual */

void
__sfp_handle_exceptions (int _fex)
{
  /* Raise exceptions represented by _FEX.  But we must raise only one
     signal at a time.  It is important that if the overflow/underflow
     exception and the divide by zero exception are given at the same
     time, the overflow/underflow exception follows the divide by zero
     exception.  */

  /* We do these bits in assembly to be certain GCC doesn't optimize
     away something important, and so we can force delayed traps to
     occur. */

  /* We use "fldd 0(%%sr0,%%sp),%0" to flush the delayed exception */

  /* First: Invalid exception.  */
  if (_fex & FP_EX_INVALID)
    {
      /* One example of an invalid operation is 0 * Infinity.  */
      double d = HUGE_VAL;
      __asm__ __volatile__ (
		"	fcpy,dbl %%fr0,%%fr22\n"
		"	fmpy,dbl %0,%%fr22,%0\n"
		"	fldd 0(%%sr0,%%sp),%0"
		: "+f" (d) : : "%fr22" );
    }

  /* Second: Division by zero.  */
  if (_fex & FP_EX_DIVZERO)
    {
      double d = 1.0;
      __asm__ __volatile__ (
		"	fcpy,dbl %%fr0,%%fr22\n"
		"	fdiv,dbl %0,%%fr22,%0\n"
		"	fldd 0(%%sr0,%%sp),%0"
		: "+f" (d) : : "%fr22" );
    }

  /* Third: Overflow.  */
  if (_fex & FP_EX_OVERFLOW)
    {
      double d = __DBL_MAX__;
      __asm__ __volatile__ (
		"	fadd,dbl %0,%0,%0\n"
		"	fldd 0(%%sr0,%%sp),%0"
		: "+f" (d) );
    }

  /* Fourth: Underflow.  */
  if (_fex & FP_EX_UNDERFLOW)
    {
      double d = __DBL_MIN__;
      double e = 3.0;
      __asm__ __volatile__ (
		"	fdiv,dbl %0,%1,%0\n"
		"	fldd 0(%%sr0,%%sp),%0"
		: "+f" (d) : "f" (e) );
    }

  /* Fifth: Inexact */
  if (_fex & FP_EX_INEXACT)
    {
      double d = 3.14159265358979323846;
      double e = 69.69;
      __asm__ __volatile__ (
		"	fdiv,dbl %0,%1,%%fr22\n"
		"	fcnvfxt,dbl,sgl %%fr22,%%fr22L\n"
		"	fldd 0(%%sr0,%%sp),%%fr22"
		: : "f" (d), "f" (e) : "%fr22" );
    }
}

/*
 * Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

void
__sfp_handle_exceptions (int _fex)
{
  const float fp_max = __FLT_MAX__;
  const float fp_min = __FLT_MIN__;
  const float fp_1e32 = 1.0e32f;
  const float fp_zero = 0.0;
  const float fp_one = 1.0;
  unsigned fpsr;

  if (_fex & FP_EX_INVALID)
    {
      __asm__ __volatile__ ("fdiv\ts0, %s0, %s0"
			    :
			    : "w" (fp_zero)
			    : "s0");
      __asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));
    }
  if (_fex & FP_EX_DIVZERO)
    {
      __asm__ __volatile__ ("fdiv\ts0, %s0, %s1"
			    :
			    : "w" (fp_one), "w" (fp_zero)
			    : "s0");
      __asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));
    }
  if (_fex & FP_EX_OVERFLOW)
    {
      __asm__ __volatile__ ("fadd\ts0, %s0, %s1"
			    :
			    : "w" (fp_max), "w" (fp_1e32)
			    : "s0");
      __asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));
    }
  if (_fex & FP_EX_UNDERFLOW)
    {
      __asm__ __volatile__ ("fmul\ts0, %s0, %s0"
			    :
			    : "w" (fp_min)
			    : "s0");
      __asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));
    }
  if (_fex & FP_EX_INEXACT)
    {
      __asm__ __volatile__ ("fsub\ts0, %s0, %s1"
			    :
			    : "w" (fp_max), "w" (fp_one)
			    : "s0");
      __asm__ __volatile__ ("mrs\t%0, fpsr" : "=r" (fpsr));
    }
}

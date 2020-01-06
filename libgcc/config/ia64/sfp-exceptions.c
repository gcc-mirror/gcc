/*
 * Copyright (C) 2012-2020 Free Software Foundation, Inc.
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
  double d;

  if (_fex & FP_EX_INVALID)
    {
      asm volatile ("frcpa.s0 %0, p1 = f0, f0" : "=f" (d) : : "p1");
    }
  if (_fex & FP_EX_DIVZERO)
    {
      asm volatile ("frcpa.s0 %0, p1 = f1, f0" : "=f" (d) : : "p1");
    }
  if (_fex & FP_EX_OVERFLOW)
    {
      d = __DBL_MAX__;
      asm volatile ("fadd.d.s0 %0 = %0, %0" : "+f" (d));
    }
  if (_fex & FP_EX_UNDERFLOW)
    {
      d = __DBL_MIN__;
      asm volatile ("fnma.d.s0 %0 = %0, %0, f0" : "+f" (d));
    }
  if (_fex & FP_EX_INEXACT)
    {
      d = __DBL_MAX__;
      asm volatile ("fsub.d.s0 %0 = %0, f1" : "+f" (d));
    }
}

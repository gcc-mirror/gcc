/* Copyright (C) 2001-2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "sfp-machine.h"

#define __math_force_eval_div(x, y) \
  do { asm ("" : "+f" (x)); asm volatile ("" : : "f" (x / y)); } while (0)

void
__sfp_handle_exceptions (int _fex)
{
  if (_fex & FP_EX_INVALID)
    {
      float x = 0.0f;
      __math_force_eval_div (x, x);
    }
  if (_fex & FP_EX_DIVZERO)
    {
      float x = 1.0f;
      float y = 0.0f;
      __math_force_eval_div (x, y);
    }
  if (_fex & FP_EX_OVERFLOW)
    {
      float x = __FLT_MAX__;
      asm ("" : "+f" (x));
      asm volatile ("" : : "f" (x + x));
    }
  if (_fex & FP_EX_UNDERFLOW)
    {
      float x = __FLT_MIN__;
      asm ("" : "+f" (x));
      asm volatile ("" : : "f" (x * x));
    }
  if (_fex & FP_EX_INEXACT)
    {
      float x = 1.0f;
      float y = 3.0f;
      __math_force_eval_div (x, y);
    }
}

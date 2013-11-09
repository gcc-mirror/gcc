/*
 * Copyright (C) 2012-2013 Free Software Foundation, Inc.
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

#ifndef _SOFT_FLOAT
#include "sfp-machine.h"

struct fenv
{
  unsigned short int __control_word;
  unsigned short int __unused1;
  unsigned short int __status_word;
  unsigned short int __unused2;
  unsigned short int __tags;
  unsigned short int __unused3;
  unsigned int __eip;
  unsigned short int __cs_selector;
  unsigned int __opcode:11;
  unsigned int __unused4:5;
  unsigned int __data_offset;
  unsigned short int __data_selector;
  unsigned short int __unused5;
};

void
__sfp_handle_exceptions (int _fex)
{
  if (_fex & FP_EX_INVALID)
    {
      float f = 0.0f;
#ifdef __x86_64__
      volatile float r __attribute__ ((unused));
      asm volatile ("%vdivss\t{%0, %d0|%d0, %0}" : "+x" (f));
      r = f; /* Needed to trigger exception.   */
#else
      asm volatile ("fdiv\t{%y0, %0|%0, %y0}" : "+t" (f));
      /* No need for fwait, exception is triggered by emitted fstp.  */
#endif
    }
  if (_fex & FP_EX_DENORM)
    {
      struct fenv temp;
      asm volatile ("fnstenv\t%0" : "=m" (temp));
      temp.__status_word |= FP_EX_DENORM;
      asm volatile ("fldenv\t%0" : : "m" (temp));
      asm volatile ("fwait");
    }
  if (_fex & FP_EX_DIVZERO)
    {
      float f = 1.0f, g = 0.0f;
#ifdef __x86_64__
      volatile float r __attribute__ ((unused));
      asm volatile ("%vdivss\t{%1, %d0|%d0, %1}" : "+x" (f) : "xm" (g));
      r = f; /* Needed to trigger exception.   */
#else
      asm volatile ("fdivs\t%1" : "+t" (f) : "m" (g));
      /* No need for fwait, exception is triggered by emitted fstp.  */
#endif
    }
  if (_fex & FP_EX_OVERFLOW)
    {
      struct fenv temp;
      asm volatile ("fnstenv\t%0" : "=m" (temp));
      temp.__status_word |= FP_EX_OVERFLOW;
      asm volatile ("fldenv\t%0" : : "m" (temp));
      asm volatile ("fwait");
    }
  if (_fex & FP_EX_UNDERFLOW)
    {
      struct fenv temp;
      asm volatile ("fnstenv\t%0" : "=m" (temp));
      temp.__status_word |= FP_EX_UNDERFLOW;
      asm volatile ("fldenv\t%0" : : "m" (temp));
      asm volatile ("fwait");
    }
  if (_fex & FP_EX_INEXACT)
    {
      float f = 1.0f, g = 3.0f;
#ifdef __x86_64__
      volatile float r __attribute__ ((unused));
      asm volatile ("%vdivss\t{%1, %d0|%d0, %1}" : "+x" (f) : "xm" (g));
      r = f; /* Needed to trigger exception.   */
#else
      asm volatile ("fdivs\t%1" : "+t" (f) : "m" (g));
      /* No need for fwait, exception is triggered by emitted fstp.  */
#endif
    }
};
#endif

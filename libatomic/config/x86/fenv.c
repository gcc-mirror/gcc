/* Copyright (C) 2013-2014 Free Software Foundation, Inc.

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

#include "libatomic_i.h"

#define FE_INVALID	0x01
#define FE_DENORM	0x02
#define FE_DIVBYZERO	0x04
#define FE_OVERFLOW	0x08
#define FE_UNDERFLOW	0x10
#define FE_INEXACT	0x20

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

/* Raise the supported floating-point exceptions from EXCEPTS.  Other
   bits in EXCEPTS are ignored.  */

void
__atomic_feraiseexcept (int excepts)
{
  if (excepts & FE_INVALID)
    {
      float f = 0.0f;
#ifdef __SSE_MATH__
      volatile float r __attribute__ ((unused));
      asm volatile ("%vdivss\t{%0, %d0|%d0, %0}" : "+x" (f));
      r = f; /* Needed to trigger exception.   */
#else
      asm volatile ("fdiv\t{%y0, %0|%0, %y0}" : "+t" (f));
      /* No need for fwait, exception is triggered by emitted fstp.  */
#endif
    }
  if (excepts & FE_DENORM)
    {
      struct fenv temp;
      asm volatile ("fnstenv\t%0" : "=m" (temp));
      temp.__status_word |= FE_DENORM;
      asm volatile ("fldenv\t%0" : : "m" (temp));
      asm volatile ("fwait");
    }
  if (excepts & FE_DIVBYZERO)
    {
      float f = 1.0f, g = 0.0f;
#ifdef __SSE_MATH__
      volatile float r __attribute__ ((unused));
      asm volatile ("%vdivss\t{%1, %d0|%d0, %1}" : "+x" (f) : "xm" (g));
      r = f; /* Needed to trigger exception.   */
#else
      asm volatile ("fdivs\t%1" : "+t" (f) : "m" (g));
      /* No need for fwait, exception is triggered by emitted fstp.  */
#endif
    }
  if (excepts & FE_OVERFLOW)
    {
      struct fenv temp;
      asm volatile ("fnstenv\t%0" : "=m" (temp));
      temp.__status_word |= FE_OVERFLOW;
      asm volatile ("fldenv\t%0" : : "m" (temp));
      asm volatile ("fwait");
    }
  if (excepts & FE_UNDERFLOW)
    {
      struct fenv temp;
      asm volatile ("fnstenv\t%0" : "=m" (temp));
      temp.__status_word |= FE_UNDERFLOW;
      asm volatile ("fldenv\t%0" : : "m" (temp));
      asm volatile ("fwait");
    }
  if (excepts & FE_INEXACT)
    {
      float f = 1.0f, g = 3.0f;
#ifdef __SSE_MATH__
      volatile float r __attribute__ ((unused));
      asm volatile ("%vdivss\t{%1, %d0|%d0, %1}" : "+x" (f) : "xm" (g));
      r = f; /* Needed to trigger exception.   */
#else
      asm volatile ("fdivs\t%1" : "+t" (f) : "m" (g));
      /* No need for fwait, exception is triggered by emitted fstp.  */
#endif
    }
}

/* Copyright (C) 1989-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "soft-fp.h"
#include "quad-float128.h"

/* __powikf3 can be compiled 3 different ways:

   1) If the assembler does not have support for the IEEE 128-bit insns
   (xsaddqp, etc.) it is just compiled as __powikf2.

   2) If the assembler has IEEE 128-bit floating point support, and __powikf2
   is not previously defined, it is defined as __powikf2_sw.

   3) If the assembler has IEEE 128-bit floating point support, and __powikf2
   is included by _powikf2-hw.c, which defines __powikf2 as __powikf2_hw.  The
   __powikf2_hw.c is compiled with -mcpu=power9 and it automatically uses the
   IEEE 128-bit instructions.

   For #2/#3, float128-ifunc.c defines an ifunc function for __powikf2, that
   will use the software version on power7/power8 systems, and the hardware
   version on power9 systems.

   The code is cloned from the code in libgcc2.c (which handles the standard
   SF, DF, TF, and XF types).  */

#if defined(FLOAT128_HW_INSNS) && !defined(__powikf2)
#define __powikf2 __powikf2_sw
#endif

TFtype
__powikf2 (TFtype x, SItype_ppc m)
{
  unsigned int n = m < 0 ? -m : m;
  TFtype y = n % 2 ? x : 1;
  while (n >>= 1)
    {
      x = x * x;
      if (n % 2)
	y = y * x;
    }
  return m < 0 ? 1/y : y;
}

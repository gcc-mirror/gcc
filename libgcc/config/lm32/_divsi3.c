/* _divsi3 for Lattice Mico32.
   Contributed by Jon Beniston <jon@beniston.com> 
   
   Copyright (C) 2009 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.
   
   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.
   
   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>. */

#include "libgcc_lm32.h"

/* Signed integer division.  */

static const UQItype __divsi3_table[] = {
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 4, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 5, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 6, 3, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 7, 3, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 8, 4, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 9, 4, 3, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
  0, 10, 5, 3, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
  0, 11, 5, 3, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
  0, 12, 6, 4, 3, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0,
  0, 13, 6, 4, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 0, 0,
  0, 14, 7, 4, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 0,
  0, 15, 7, 5, 3, 3, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1,
};

SItype
__divsi3 (SItype a, SItype b)
{
  int neg = 0;
  SItype res;
  int cfg;

  if (b == 0)
    {
      /* Raise divide by zero exception.  */
      int eba, sr;
      /* Save interrupt enable.  */
      __asm__ __volatile__ ("rcsr %0, IE":"=r" (sr));
      sr = (sr & 1) << 1;
      __asm__ __volatile__ ("wcsr IE, %0"::"r" (sr));
      /* Branch to exception handler.  */
      __asm__ __volatile__ ("rcsr %0, EBA":"=r" (eba));
      eba += 32 * 5;
      __asm__ __volatile__ ("mv ea, ra");
      __asm__ __volatile__ ("b %0"::"r" (eba));
      __builtin_unreachable ();
    }

  if (((USItype) (a | b)) < 16)
    res = __divsi3_table[(a << 4) + b];
  else
    {

      if (a < 0)
	{
	  a = -a;
	  neg = !neg;
	}

      if (b < 0)
	{
	  b = -b;
	  neg = !neg;
	}

    __asm__ ("rcsr %0, CFG":"=r" (cfg));
      if (cfg & 2)
      __asm__ ("divu %0, %1, %2": "=r" (res):"r" (a), "r" (b));
      else
	res = __udivmodsi4 (a, b, 0);

      if (neg)
	res = -res;
    }

  return res;
}

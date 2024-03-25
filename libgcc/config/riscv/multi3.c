/* Multiplication two double word integers for RISC-V.

   Copyright (C) 2016-2024 Free Software Foundation, Inc.

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

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#define LIBGCC2_UNITS_PER_WORD (__riscv_xlen / 8)

#include "libgcc2.h"

#if __riscv_xlen == 32
/* Our RV64 64-bit routines are equivalent to our RV32 32-bit routines.  */
# define __multi3 __muldi3
#endif

DWtype
__multi3 (DWtype u, DWtype v)
{
  const DWunion uu = {.ll = u};
  const DWunion vv = {.ll = v};
  DWunion w;
  UWtype u_low = uu.s.low;
  UWtype v_low = vv.s.low;
  UWtype u_low_msb;
  UWtype w_low = 0;
  UWtype new_w_low;
  UWtype w_high = 0;
  UWtype w_high_tmp = 0;
  UWtype w_high_tmp2x;
  UWtype carry;

  /* Calculate low half part of u and v, and get a UDWtype result just like
     what __umulsidi3 do.  */
  do
    {
      new_w_low = w_low + u_low;
      w_high_tmp2x = w_high_tmp << 1;
      w_high_tmp += w_high;
      if (v_low & 1)
	{
	  carry = new_w_low < w_low;
	  w_low = new_w_low;
	  w_high = carry + w_high_tmp;
	}
      u_low_msb = (u_low >> ((sizeof (UWtype) * 8) - 1));
      v_low >>= 1;
      u_low <<= 1;
      w_high_tmp = u_low_msb | w_high_tmp2x;
    }
  while (v_low);

  w.s.low = w_low;
  w.s.high = w_high;

  if (uu.s.high)
    w.s.high = w.s.high + __muluw3(vv.s.low, uu.s.high);

  if (vv.s.high)
    w.s.high += __muluw3(uu.s.low, vv.s.high);

  return w.ll;
}

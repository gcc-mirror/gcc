/* Copyright (C) 2006-2017 Free Software Foundation, Inc.
  
   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.
  
   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.
  
   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include <spu_intrinsics.h>
const unsigned char __sidf_pat[16] __attribute__ ((__aligned__ (16))) = {
  0x02, 0x03, 0x10, 0x11,
  0x12, 0x13, 0x80, 0x80,
  0x06, 0x07, 0x14, 0x15,
  0x16, 0x17, 0x80, 0x80
};

/* double __float_unssidf (unsigned int SI) */
qword __float_unssidf (qword SI);
qword
__float_unssidf (qword SI)
{
  qword t0, t1, t2, t3, t4, t5, t6, t7;
  t0 = si_clz (SI);
  t1 = si_il (1054);
  t2 = si_shl (SI, t0);
  t3 = si_ceqi (t0, 32);
  t4 = si_sf (t0, t1);
  t5 = si_a (t2, t2);
  t6 = si_andc (t4, t3);
  t7 = si_shufb (t6, t5, *(const qword *) __sidf_pat);
  return si_shlqbii (t7, 4);
}

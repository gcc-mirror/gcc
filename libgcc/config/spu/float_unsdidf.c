/* Copyright (C) 2006-2014 Free Software Foundation, Inc.
  
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
const unsigned char __didf_scale[16] __attribute__ ((__aligned__ (16))) = {
  0x00, 0x00, 0x04, 0x3e,
  0x00, 0x00, 0x04, 0x1e,
  0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00
};
const unsigned char __didf_pat[16] __attribute__ ((__aligned__ (16))) = {
  0x02, 0x03, 0x10, 0x11,
  0x12, 0x13, 0x80, 0x80,
  0x06, 0x07, 0x14, 0x15,
  0x16, 0x17, 0x80, 0x80
};

/* double __float_unsdidf (unsigned long long int) 
   Construct two exact doubles representing the high and low parts (in
   parallel), then add them. */
qword __float_unsdidf (qword DI);
qword
__float_unsdidf (qword DI)
{
  qword t0, t1, t2, t3, t4, t5, t6, t7, t8;
  t0 = si_clz (DI);
  t1 = si_shl (DI, t0);
  t2 = si_ceqi (t0, 32);
  t3 = si_sf (t0, *(const qword *) __didf_scale);
  t4 = si_a (t1, t1);
  t5 = si_andc (t3, t2);
  t6 = si_shufb (t5, t4, *(const qword *) __didf_pat);
  t7 = si_shlqbii (t6, 4);
  t8 = si_shlqbyi (t7, 8);
  return si_dfa (t7, t8);
}

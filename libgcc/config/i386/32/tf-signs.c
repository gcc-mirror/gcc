/* Copyright (C) 2008-2021 Free Software Foundation, Inc.

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

union _FP_UNION_Q
{
   __float128 flt;
   struct 
   {
      unsigned long frac0 : 32;
      unsigned long frac1 : 32;
      unsigned long frac2 : 32;
      unsigned long frac3 : 16;
      unsigned exp : 15;
      unsigned sign : 1;
   } bits __attribute__((packed));
};

__float128 __copysigntf3 (__float128, __float128);
__float128 __fabstf2 (__float128);

__float128
__copysigntf3 (__float128 a, __float128 b)
{
  union _FP_UNION_Q A, B;

  A.flt = a;
  B.flt = b;
  A.bits.sign = B.bits.sign;

  return A.flt;
}

__float128
__fabstf2 (__float128 a)
{
  union _FP_UNION_Q A;

  A.flt = a;
  A.bits.sign = 0;

  return A.flt;
}

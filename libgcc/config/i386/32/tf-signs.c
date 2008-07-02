/* Copyright (C) 2008 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

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

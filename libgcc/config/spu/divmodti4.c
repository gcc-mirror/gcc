/* Copyright (C) 2008-2013 Free Software Foundation, Inc.
 
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

typedef unsigned int UTItype __attribute__ ((mode (TI)));
typedef int TItype __attribute__ ((mode (TI)));
TItype __divti3 (TItype u, TItype v);
TItype __modti3 (TItype u, TItype v);
UTItype __udivti3 (UTItype u, UTItype v);
UTItype __umodti3 (UTItype u, UTItype v);
UTItype __udivmodti4 (UTItype u, UTItype v, UTItype *w);

union qword_UTItype
  {
    qword q;
    UTItype t;
  };
  
inline static qword
si_from_UTItype (UTItype t)
{ 
  union qword_UTItype u;
  u.t = t;
  return u.q;
}

inline static UTItype
si_to_UTItype (qword q)
{ 
  union qword_UTItype u;
  u.q = q;
  return u.t;
}

inline static unsigned int
count_leading_zeros (UTItype x)
{
  qword c = si_clz (*(qword *) & x);
  qword cmp0 = si_cgti (c, 31);
  qword cmp1 = si_and (cmp0, si_shlqbyi (cmp0, 4));
  qword cmp2 = si_and (cmp1, si_shlqbyi (cmp0, 8));
  qword s = si_a (c, si_and (cmp0, si_shlqbyi (c, 4)));
  s = si_a (s, si_and (cmp1, si_shlqbyi (c, 8)));
  s = si_a (s, si_and (cmp2, si_shlqbyi (c, 12)));
  return si_to_uint (s);
}

/* Based on implementation of udivmodsi4, which is essentially
 * an optimized version of libgcc/udivmodsi4.c
        clz      %7,%2
        clz      %4,%1
        il       %5,1
        fsmbi    %0,0
        sf       %7,%4,%7
        ori      %3,%1,0
        shl      %5,%5,%7
        shl      %4,%2,%7
1:      or       %8,%0,%5
        rotmi    %5,%5,-1
        clgt     %6,%4,%3
        sf       %7,%4,%3
        rotmi    %4,%4,-1
        selb     %0,%8,%0,%6
        selb     %3,%7,%3,%6
3:      brnz     %5,1b
 */

UTItype
__udivmodti4 (UTItype num, UTItype den, UTItype * rp)
{
  qword shift =
    si_from_uint (count_leading_zeros (den) - count_leading_zeros (num));
  qword n0 = si_from_UTItype (num);
  qword d0 = si_from_UTItype (den);
  qword bit = si_andi (si_fsmbi (1), 1);
  qword r0 = si_il (0);
  qword m1 = si_fsmbi (0x000f);
  qword mask, r1, n1;

  d0 = si_shlqbybi (si_shlqbi (d0, shift), shift);
  bit = si_shlqbybi (si_shlqbi (bit, shift), shift);

  do
    {
      r1 = si_or (r0, bit);

      // n1 = n0 - d0 in TImode
      n1 = si_bg (d0, n0);
      n1 = si_shlqbyi (n1, 4);
      n1 = si_sf (m1, n1);
      n1 = si_bgx (d0, n0, n1);
      n1 = si_shlqbyi (n1, 4);
      n1 = si_sf (m1, n1);
      n1 = si_bgx (d0, n0, n1);
      n1 = si_shlqbyi (n1, 4);
      n1 = si_sf (m1, n1);
      n1 = si_sfx (d0, n0, n1);

      mask = si_fsm (si_cgti (n1, -1));
      r0 = si_selb (r0, r1, mask);
      n0 = si_selb (n0, n1, mask);
      bit = si_rotqmbii (bit, -1);
      d0 = si_rotqmbii (d0, -1);
    }
  while (si_to_uint (si_orx (bit)));
  if (rp)
    *rp = si_to_UTItype (n0);
  return si_to_UTItype (r0);
}

UTItype
__udivti3 (UTItype n, UTItype d)
{
  return __udivmodti4 (n, d, (UTItype *)0);
}

UTItype
__umodti3 (UTItype n, UTItype d)
{
  UTItype w;
  __udivmodti4 (n, d, &w);
  return w;
}

TItype
__divti3 (TItype n, TItype d)
{
  int c = 0;
  TItype w;

  if (n < 0)
    {
        c = ~c;
        n = -n;
    }
  if (d < 0)
    {
        c = ~c;
        d = -d;
    }

  w = __udivmodti4 (n, d, (UTItype *)0);
  if (c)
    w = -w;
  return w;
}

TItype
__modti3 (TItype n, TItype d)
{
  int c = 0;
  TItype w;

  if (n < 0)
    {
        c = ~c;
        n = -n;
    }
  if (d < 0)
    {
        c = ~c;
        d = -d;
    }

  __udivmodti4 (n, d, (UTItype *) &w);
  if (c)
    w = -w;
  return w;
}

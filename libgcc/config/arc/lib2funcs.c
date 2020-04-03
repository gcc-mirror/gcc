/* libgcc routines for ARC
   Copyright (C) 2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */


typedef          int  sint64_t   __attribute__ ((mode (DI)));
typedef unsigned int  uint64_t   __attribute__ ((mode (DI)));
typedef unsigned int  nint32_t   __attribute__ ((mode (SI)));
typedef int           word_t     __attribute__ ((mode (__word__)));

sint64_t __muldi3 (sint64_t, sint64_t);
nint32_t __umodsi3 (nint32_t, nint32_t);

#ifdef __ARC_RF16__

/* Generic multiplication procedure. No mpy operation involved.  */
sint64_t
__muldi3 (sint64_t a, sint64_t b)
{
  sint64_t res = 0;
  uint64_t cnt = a;

  while (cnt)
    {
      if (cnt & 1)
        res += b;
      b <<= 1;
      cnt >>= 1;
    }
  return res;
}

/* Unsigned 32bit integer division/modulus.  */

static inline __attribute__ ((__always_inline__))
nint32_t
udivmodsi4 (nint32_t num, nint32_t den, word_t modwanted)
{
  nint32_t bit = 1;
  nint32_t res = 0;

  while (den < num && bit && !(den & (1LL << 63)))
    {
      den <<= 1;
      bit <<= 1;
    }
  while (bit)
    {
      if (num >= den)
	{
	  num -= den;
	  res |= bit;
	}
      bit >>= 1;
      den >>= 1;
    }
  if (modwanted)
    return num;
  return res;
}

nint32_t
__umodsi3 (nint32_t a, nint32_t b)
{
  return udivmodsi4 (a, b, 1);
}

#endif

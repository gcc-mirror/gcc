/* libgcc routines for PRU.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Based on rl78/rl78-mul.h.

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

UINT_TYPE C2(__pruabi_softmpy,NAME_MODE)   (UINT_TYPE, UINT_TYPE);
UINT_TYPE
C2(__pruabi_softmpy,NAME_MODE) (UINT_TYPE a, UINT_TYPE b)
{
  UINT_TYPE rv = 0;

  unsigned char bit;

  for (bit=0; b && bit<sizeof(UINT_TYPE)*8; bit++)
    {
      if (b & 1)
	rv += a;
      a <<= 1;
      b >>= 1;
    }
  return rv;
}

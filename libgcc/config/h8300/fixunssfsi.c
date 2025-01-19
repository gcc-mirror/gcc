/* More subroutines needed by GCC output code on some machines.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* The libgcc2.c implementation gets confused by our type setup and creates
   a directly recursive call, so we do our own implementation.  For
   the H8/300, that's in lib1funcs.S, for H8/300H and H8S, it's here.  */

#ifndef __H8300__
long __fixunssfsi (float a);

long
__fixunssfsi (float a)
{
  if (a >= (float) 32768L)
    return (long) (a - 32768L) + 32768L;
  return (long) a;
}
#endif

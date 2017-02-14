/* Software IEEE 128-bit floating-point emulation for PowerPC.

   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Michael Meissner (meissner@linux.vnet.ibm.com)
   Code is based on the main soft-fp library written by:
	Richard Henderson (rth@cygnus.com) and
	Jakub Jelinek (jj@ultra.linux.cz).

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   In addition to the permissions in the GNU Lesser General Public
   License, the Free Software Foundation gives you unlimited
   permission to link the compiled version of this file into
   combinations with other programs, and to distribute those
   combinations without any restriction coming from the use of this
   file.  (The Lesser General Public License restrictions do apply in
   other respects; for example, they cover modification of the file,
   and distribution when not linked into a combine executable.)

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/* Convert IBM long double to IEEE 128-bit floating point.  */

#ifdef __FLOAT128_HARDWARE__
#error "This module must not be compiled with IEEE 128-bit hardware support"
#endif

#include "soft-fp.h"
#include "quad-float128.h"

#ifndef FLOAT128_HW_INSNS
#define __trunctfkf2_sw __trunctfkf2
#endif

__float128
__trunctfkf2_sw (IBM128_TYPE value)
{
  __float128 ret;

  CVT_IBM128_TO_FLOAT128 (ret, value);
  return ret;
}

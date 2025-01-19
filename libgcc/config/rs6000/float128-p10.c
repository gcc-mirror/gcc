/* Automatic switching between software and hardware IEEE 128-bit
   ISA 3.1 floating-point emulation for PowerPC.

   Copyright (C) 2016-2025 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Carl Love (cel@us.ibm.com)
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

/* Note, the hardware conversion instructions for 128-bit integers are
   supported for ISA 3.1 and later.  Only compile this file with -mcpu=power10
   or newer support.  */

#include <soft-fp.h>
#include <quad-float128.h>

#ifndef __FLOAT128_HARDWARE__
#error "This module must be compiled with IEEE 128-bit hardware support"
#endif

#ifndef _ARCH_PWR10
#error "This module must be compiled for Power 10 support"
#endif

TFtype
__floattikf_hw (TItype_ppc a)
{
  return (TFtype) a;
}

TFtype
__floatuntikf_hw (UTItype_ppc a)
{
  return (TFtype) a;
}

TItype_ppc
__fixkfti_hw (TFtype a)
{
  return (TItype_ppc) a;
}

UTItype_ppc
__fixunskfti_hw (TFtype a)
{
  return (UTItype_ppc) a;
}

/* Automatic switching between software and hardware IEEE 128-bit
   floating-point emulation for PowerPC.

   Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

#include <soft-fp.h>
#include <quad-float128.h>

#ifndef __FLOAT128_HARDWARE__
#error "This module must be compiled with IEEE 128-bit hardware support"
#endif

TFtype
__addkf3_hw (TFtype a, TFtype b)
{
  return a + b;
}

TFtype
__subkf3_hw (TFtype a, TFtype b)
{
  return a - b;
}

TFtype
__mulkf3_hw (TFtype a, TFtype b)
{
  return a * b;
}

TFtype
__divkf3_hw (TFtype a, TFtype b)
{
  return a / b;
}

TFtype
__negkf2_hw (TFtype a)
{
  return -a;
}

TFtype
__floatsikf_hw (SItype_ppc a)
{
  return (TFtype) a;
}

TFtype
__floatunsikf_hw (USItype_ppc a)
{
  return (TFtype) a;
}

TFtype
__floatdikf_hw (DItype_ppc a)
{
  return (TFtype) a;
}

TFtype
__floatundikf_hw (UDItype_ppc a)
{
  return (TFtype) a;
}

SItype_ppc
__fixkfsi_hw (TFtype a)
{
  return (SItype_ppc) a;
}

USItype_ppc
__fixunskfsi_hw (TFtype a)
{
  return (USItype_ppc) a;
}

DItype_ppc
__fixkfdi_hw (TFtype a)
{
  return (DItype_ppc) a;
}

UDItype_ppc
__fixunskfdi_hw (TFtype a)
{
  return (UDItype_ppc) a;
}

TFtype
__extendsfkf2_hw (float a)
{
  return (TFtype) a;
}

TFtype
__extenddfkf2_hw (double a)
{
  return (TFtype) a;
}

float
__trunckfsf2_hw (TFtype a)
{
  return (float) a;
}

double
__trunckfdf2_hw (TFtype a)
{
  return (double) a;
}

/* __eqkf2 returns 0 if equal, or 1 if not equal or NaN.  */
CMPtype
__eqkf2_hw (TFtype a, TFtype b)
{
  return (a != b);
}

/* __gekf2 returns -1 if a < b, 0 if a == b, +1 if a > b, or -2 if NaN.  */
CMPtype
__gekf2_hw (TFtype a, TFtype b)
{
  if (a < b)
    return -1;

  else if (__builtin_isunordered (a, b))
    return -2;

  else if (a == b)
    return 0;

  return 1;
}

/* __lekf2 returns -1 if a < b, 0 if a == b, +1 if a > b, or +2 if NaN.  */
CMPtype
__lekf2_hw (TFtype a, TFtype b)
{
  if (a < b)
    return -1;

  else if (__builtin_isunordered (a, b))
    return 2;

  else if (a == b)
    return 0;

  return 1;
}

/* __unordkf2 returns 1 if NaN or 0 otherwise.  */
CMPtype
__unordkf2_hw (TFtype a, TFtype b)
{
  return (__builtin_isunordered (a, b)) ? 1 : 0;
}

/* Convert __float128 to __ibm128.  */
IBM128_TYPE
__extendkftf2_hw (TFtype value)
{
  IBM128_TYPE ret;

  CVT_FLOAT128_TO_IBM128 (ret, value);
  return ret;
}

/* Convert __ibm128 to __float128.  */
TFtype
__trunctfkf2_hw (IBM128_TYPE value)
{
  TFtype ret;

  CVT_IBM128_TO_FLOAT128 (ret, value);
  return ret;
}

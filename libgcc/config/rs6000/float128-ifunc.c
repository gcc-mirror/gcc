/* Automatic switching between software and hardware IEEE 128-bit
   floating-point emulation for PowerPC.

   Copyright (C) 2016-2020 Free Software Foundation, Inc.
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
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#ifndef FLOAT128_HW_INSNS
#error "float128-ifunc.c needs access to ISA 3.0 instructions and ifunc"
#endif

#ifdef __FLOAT128_HARDWARE__
#error "This module must not be compiled with IEEE 128-bit hardware support"
#endif

#define SW_OR_HW(SW, HW) (__builtin_cpu_supports ("ieee128") ? HW : SW)

/* Resolvers.  */

/* We do not provide ifunc resolvers for __fixkfti, __fixunskfti, __floattikf,
   and __floatuntikf.  There is no ISA 3.0 instruction that converts between
   128-bit integer types and 128-bit IEEE floating point, or vice versa.  So
   use the emulator functions for these conversions.  */

static __typeof__ (__addkf3_sw) *
__addkf3_resolve (void)
{
  return SW_OR_HW (__addkf3_sw, __addkf3_hw);
}

static __typeof__ (__subkf3_sw) *
__subkf3_resolve (void)
{
  return SW_OR_HW (__subkf3_sw, __subkf3_hw);
}

static __typeof__ (__mulkf3_sw) *
__mulkf3_resolve (void)
{
  return SW_OR_HW (__mulkf3_sw, __mulkf3_hw);
}

static __typeof__ (__divkf3_sw) *
__divkf3_resolve (void)
{
  return SW_OR_HW (__divkf3_sw, __divkf3_hw);
}

static __typeof__ (__negkf2_sw) *
__negkf2_resolve (void)
{
  return SW_OR_HW (__negkf2_sw, __negkf2_hw);
}

static __typeof__ (__powikf2_sw) *
__powikf2_resolve (void)
{
  return SW_OR_HW (__powikf2_sw, __powikf2_hw);
}

static __typeof__ (__floatsikf_sw) *
__floatsikf_resolve (void)
{
  return SW_OR_HW (__floatsikf_sw, __floatsikf_hw);
}

static __typeof__ (__floatdikf_sw) *
__floatdikf_resolve (void)
{
  return SW_OR_HW (__floatdikf_sw, __floatdikf_hw);
}

static __typeof__ (__floatunsikf_sw) *
__floatunsikf_resolve (void)
{
  return SW_OR_HW (__floatunsikf_sw, __floatunsikf_hw);
}

static __typeof__ (__floatundikf_sw) *
__floatundikf_resolve (void)
{
  return SW_OR_HW (__floatundikf_sw, __floatundikf_hw);
}

static __typeof__ (__fixkfsi_sw) *
__fixkfsi_resolve (void)
{
  return SW_OR_HW (__fixkfsi_sw, __fixkfsi_hw);
}

static __typeof__ (__fixkfdi_sw) *
__fixkfdi_resolve (void)
{
  return SW_OR_HW (__fixkfdi_sw, __fixkfdi_hw);
}

static __typeof__ (__fixunskfsi_sw) *
__fixunskfsi_resolve (void)
{
  return SW_OR_HW (__fixunskfsi_sw, __fixunskfsi_hw);
}

static __typeof__ (__fixunskfdi_sw) *
__fixunskfdi_resolve (void)
{
  return SW_OR_HW (__fixunskfdi_sw, __fixunskfdi_hw);
}

static __typeof__ (__extendsfkf2_sw) *
__extendsfkf2_resolve (void)
{
  return SW_OR_HW (__extendsfkf2_sw, __extendsfkf2_hw);
}

static __typeof__ (__extenddfkf2_sw) *
__extenddfkf2_resolve (void)
{
  return SW_OR_HW (__extenddfkf2_sw, __extenddfkf2_hw);
}

static __typeof__ (__trunckfsf2_sw) *
__trunckfsf2_resolve (void)
{
  return SW_OR_HW (__trunckfsf2_sw, __trunckfsf2_hw);
}

static __typeof__ (__trunckfdf2_sw) *
__trunckfdf2_resolve (void)
{
  return (void *) SW_OR_HW (__trunckfdf2_sw, __trunckfdf2_hw);
}

static __typeof__ (__extendkftf2_sw) *
__extendkftf2_resolve (void)
{
  return SW_OR_HW (__extendkftf2_sw, __extendkftf2_hw);
}

static __typeof__ (__trunctfkf2_sw) *
__trunctfkf2_resolve (void)
{
  return SW_OR_HW (__trunctfkf2_sw, __trunctfkf2_hw);
}

static __typeof__ (__mulkc3_sw) *
__mulkc3_resolve (void)
{
  return SW_OR_HW (__mulkc3_sw, __mulkc3_hw);
}

static __typeof__ (__divkc3_sw) *
__divkc3_resolve (void)
{
  return SW_OR_HW (__divkc3_sw, __divkc3_hw);
}

static __typeof__ (__eqkf2_sw) *
__eqkf2_resolve (void)
{
  return SW_OR_HW (__eqkf2_sw, __eqkf2_hw);
}

static __typeof__ (__gekf2_sw) *
__gekf2_resolve (void)
{
  return SW_OR_HW (__gekf2_sw, __gekf2_hw);
}

static __typeof__ (__lekf2_sw) *
__lekf2_resolve (void)
{
  return SW_OR_HW (__lekf2_sw, __lekf2_hw);
}

static __typeof__ (__unordkf2_sw) *
__unordkf2_resolve (void)
{
  return SW_OR_HW (__unordkf2_sw, __unordkf2_hw);
}

/* Resolve __nekf2, __gtkf2, __ltkf2 like __eqkf2, __gekf2, and __lekf2, since
   the functions return the same values.  */

static __typeof__ (__eqkf2_sw) *
__nekf2_resolve (void)
{
  return SW_OR_HW (__eqkf2_sw, __eqkf2_hw);
}

static __typeof__ (__eqkf2_sw) *
__gtkf2_resolve (void)
{
  return SW_OR_HW (__gekf2_sw, __gekf2_hw);
}

static __typeof__ (__eqkf2_sw) *
__ltkf2_resolve (void)
{
  return SW_OR_HW (__lekf2_sw, __lekf2_hw);
}



/* Ifunc definitions.  */
TFtype __addkf3 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__addkf3_resolve")));

TFtype __subkf3 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__subkf3_resolve")));

TFtype __mulkf3 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__mulkf3_resolve")));

TFtype __divkf3 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__divkf3_resolve")));

TFtype __negkf2 (TFtype)
  __attribute__ ((__ifunc__ ("__negkf2_resolve")));

TFtype __powikf2 (TFtype, SItype_ppc)
  __attribute__ ((__ifunc__ ("__powikf2_resolve")));

CMPtype __eqkf2 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__eqkf2_resolve")));

CMPtype __nekf2 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__nekf2_resolve")));

CMPtype __gekf2 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__gekf2_resolve")));

CMPtype __gtkf2 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__gtkf2_resolve")));

CMPtype __lekf2 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__lekf2_resolve")));

CMPtype __ltkf2 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__ltkf2_resolve")));

CMPtype __unordkf2 (TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__unordkf2_resolve")));

TFtype __extendsfkf2 (float)
  __attribute__ ((__ifunc__ ("__extendsfkf2_resolve")));

TFtype __extenddfkf2 (double)
  __attribute__ ((__ifunc__ ("__extenddfkf2_resolve")));

float __trunckfsf2 (TFtype)
  __attribute__ ((__ifunc__ ("__trunckfsf2_resolve")));

double __trunckfdf2 (TFtype)
  __attribute__ ((__ifunc__ ("__trunckfdf2_resolve")));

SItype_ppc __fixkfsi (TFtype)
  __attribute__ ((__ifunc__ ("__fixkfsi_resolve")));

DItype_ppc __fixkfdi (TFtype)
  __attribute__ ((__ifunc__ ("__fixkfdi_resolve")));

USItype_ppc __fixunskfsi (TFtype)
  __attribute__ ((__ifunc__ ("__fixunskfsi_resolve")));

UDItype_ppc __fixunskfdi (TFtype)
  __attribute__ ((__ifunc__ ("__fixunskfdi_resolve")));

TFtype __floatsikf (SItype_ppc)
  __attribute__ ((__ifunc__ ("__floatsikf_resolve")));

TFtype __floatdikf (DItype_ppc)
  __attribute__ ((__ifunc__ ("__floatdikf_resolve")));

TFtype __floatunsikf (USItype_ppc)
  __attribute__ ((__ifunc__ ("__floatunsikf_resolve")));

TFtype __floatundikf (UDItype_ppc)
  __attribute__ ((__ifunc__ ("__floatundikf_resolve")));

IBM128_TYPE __extendkftf2 (TFtype)
  __attribute__ ((__ifunc__ ("__extendkftf2_resolve")));

TFtype __trunctfkf2 (IBM128_TYPE)
  __attribute__ ((__ifunc__ ("__trunctfkf2_resolve")));

TCtype __mulkc3 (TFtype, TFtype, TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__mulkc3_resolve")));

TCtype __divkc3 (TFtype, TFtype, TFtype, TFtype)
  __attribute__ ((__ifunc__ ("__divkc3_resolve")));

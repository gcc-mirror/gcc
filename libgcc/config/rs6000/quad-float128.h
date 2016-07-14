/* Software floating-point emulation.
   Definitions for IEEE Quad Precision on the PowerPC.
   Copyright (C) 2016 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Michael Meissner (meissner@linux.vnet.ibm.com).

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

/* quad.h defines the TFtype type by:
   typedef float TFtype __attribute__ ((mode (TF)));

   This define forces it to use KFmode (aka, ieee 128-bit floating point).  */
#define TF KF

/* We also need TCtype to represent complex ieee 128-bit float for
   __mulkc3 and __divkc3.  */
typedef __complex float TCtype __attribute__ ((mode (KC)));

/* Force the use of the VSX instruction set.  */
#if defined(_ARCH_PPC) && (!defined(__VSX__) || !defined(__FLOAT128__))
#pragma GCC target ("vsx,float128")
#endif

#include <quad.h>

#ifdef __LONG_DOUBLE_IEEE128__
#define IBM128_TYPE		__ibm128

#else
#define IBM128_TYPE		long double
#endif

/* Add prototypes of the library functions created.  In case the appropriate
   int/long types are not declared in scope by the time quad.h is included,
   provide our own version.  */
typedef int	 SItype_ppc  __attribute__ ((__mode__ (__SI__)));
typedef int	 DItype_ppc  __attribute__ ((__mode__ (__DI__)));
typedef unsigned USItype_ppc __attribute__ ((__mode__ (__SI__)));
typedef unsigned UDItype_ppc __attribute__ ((__mode__ (__DI__)));

#ifdef _ARCH_PPC64
typedef int	 TItype_ppc  __attribute__ ((__mode__ (__TI__)));
typedef unsigned UTItype_ppc __attribute__ ((__mode__ (__TI__)));
#endif

/* Software emulation functions.  */
extern TFtype __addkf3_sw (TFtype, TFtype);
extern TFtype __subkf3_sw (TFtype, TFtype);
extern TFtype __mulkf3_sw (TFtype, TFtype);
extern TFtype __divkf3_sw (TFtype, TFtype);
extern TFtype __negkf2_sw (TFtype);
extern CMPtype __eqkf2_sw (TFtype, TFtype);
extern CMPtype __gekf2_sw (TFtype, TFtype);
extern CMPtype __lekf2_sw (TFtype, TFtype);
extern CMPtype __unordkf2_sw (TFtype, TFtype);
extern TFtype __extendsfkf2_sw (float);
extern TFtype __extenddfkf2_sw (double);
extern float __trunckfsf2_sw (TFtype);
extern double __trunckfdf2_sw (TFtype);
extern SItype_ppc __fixkfsi_sw (TFtype);
extern DItype_ppc __fixkfdi_sw (TFtype);
extern USItype_ppc __fixunskfsi_sw (TFtype);
extern UDItype_ppc __fixunskfdi_sw (TFtype);
extern TFtype __floatsikf_sw (SItype_ppc);
extern TFtype __floatdikf_sw (DItype_ppc);
extern TFtype __floatunsikf_sw (USItype_ppc);
extern TFtype __floatundikf_sw (UDItype_ppc);
extern IBM128_TYPE __extendkftf2_sw (TFtype);
extern TFtype __trunctfkf2_sw (IBM128_TYPE);

#ifdef _ARCH_PPC64
/* We do not provide ifunc resolvers for __fixkfti, __fixunskfti, __floattikf,
   and __floatuntikf.  There is no ISA 3.0 instruction that converts between
   128-bit integer types and 128-bit IEEE floating point, or vice versa.  So
   use the emulator functions for these conversions.  */

extern TItype_ppc __fixkfti (TFtype);
extern UTItype_ppc __fixunskfti (TFtype);
extern TFtype __floattikf (TItype_ppc);
extern TFtype __floatuntikf (UTItype_ppc);
#endif

/* Functions using the ISA 3.0 hardware support.  If the code is compiled with
   -mcpu=power9, it will not use these functions, but if it was compiled with
   -mcpu=power7 or -mcpu=power8 and run on a ISA 3.0 system, it will use the
   hardware instruction.  */
extern TFtype __addkf3_hw (TFtype, TFtype);
extern TFtype __subkf3_hw (TFtype, TFtype);
extern TFtype __mulkf3_hw (TFtype, TFtype);
extern TFtype __divkf3_hw (TFtype, TFtype);
extern TFtype __negkf2_hw (TFtype);
extern CMPtype __eqkf2_hw (TFtype, TFtype);
extern CMPtype __gekf2_hw (TFtype, TFtype);
extern CMPtype __lekf2_hw (TFtype, TFtype);
extern CMPtype __unordkf2_hw (TFtype, TFtype);
extern TFtype __extendsfkf2_hw (float);
extern TFtype __extenddfkf2_hw (double);
extern float __trunckfsf2_hw (TFtype);
extern double __trunckfdf2_hw (TFtype);
extern SItype_ppc __fixkfsi_hw (TFtype);
extern DItype_ppc __fixkfdi_hw (TFtype);
extern USItype_ppc __fixunskfsi_hw (TFtype);
extern UDItype_ppc __fixunskfdi_hw (TFtype);
extern TFtype __floatsikf_hw (SItype_ppc);
extern TFtype __floatdikf_hw (DItype_ppc);
extern TFtype __floatunsikf_hw (USItype_ppc);
extern TFtype __floatundikf_hw (UDItype_ppc);
extern IBM128_TYPE __extendkftf2_hw (TFtype);
extern TFtype __trunctfkf2_hw (IBM128_TYPE);

/* Ifunc function declarations, to automatically switch between software
   emulation and hardware support.  */
extern TFtype __addkf3 (TFtype, TFtype);
extern TFtype __subkf3 (TFtype, TFtype);
extern TFtype __mulkf3 (TFtype, TFtype);
extern TFtype __divkf3 (TFtype, TFtype);
extern TFtype __negkf2 (TFtype);
extern CMPtype __eqkf2 (TFtype, TFtype);
extern CMPtype __nekf2 (TFtype, TFtype);
extern CMPtype __gekf2 (TFtype, TFtype);
extern CMPtype __gtkf2 (TFtype, TFtype);
extern CMPtype __lekf2 (TFtype, TFtype);
extern CMPtype __ltkf2 (TFtype, TFtype);
extern CMPtype __unordkf2 (TFtype, TFtype);
extern TFtype __extendsfkf2 (float);
extern TFtype __extenddfkf2 (double);
extern float __trunckfsf2 (TFtype);
extern double __trunckfdf2 (TFtype);
extern SItype_ppc __fixkfsi (TFtype);
extern DItype_ppc __fixkfdi (TFtype);
extern USItype_ppc __fixunskfsi (TFtype);
extern UDItype_ppc __fixunskfdi (TFtype);
extern TFtype __floatsikf (SItype_ppc);
extern TFtype __floatdikf (DItype_ppc);
extern TFtype __floatunsikf (USItype_ppc);
extern TFtype __floatundikf (UDItype_ppc);
extern IBM128_TYPE __extendkftf2 (TFtype);
extern TFtype __trunctfkf2 (IBM128_TYPE);

/* Complex __float128 built on __float128 interfaces.  */
extern TCtype __mulkc3 (TFtype, TFtype, TFtype, TFtype);
extern TCtype __divkc3 (TFtype, TFtype, TFtype, TFtype);

/* Implementation of conversions between __ibm128 and __float128, to allow the
   same code to be used on systems with IEEE 128-bit emulation and with IEEE
   128-bit hardware support.  */

union ibm128_union {
  IBM128_TYPE ibm128;
  double dbl[2];
};

#define CVT_FLOAT128_TO_IBM128(RESULT, VALUE)				\
{									\
  double __high, __low;							\
  __float128 __value = (VALUE);						\
  union ibm128_union u;							\
									\
  __high = (double) __value;						\
  if (__builtin_isnan (__high) || __builtin_isinf (__high))		\
    __low = 0.0;							\
									\
  else									\
    {									\
      double __high_temp;						\
									\
      __low = (double) (__value - (__float128) __high);			\
      /* Renormalize low/high and move them into canonical IBM long	\
	 double form.  */						\
      __high_temp = __high + __low;					\
      __low = (__high - __high_temp) + __low;				\
      __high = __high_temp;						\
    }									\
									\
  u.dbl[0] = __high;							\
  u.dbl[1] = __low;							\
  RESULT = u.ibm128;							\
}

#define CVT_IBM128_TO_FLOAT128(RESULT, VALUE)				\
{									\
  union ibm128_union u;							\
  double __high, __low;							\
									\
  u.ibm128 = (VALUE);							\
  __high = u.dbl[0];							\
  __low = u.dbl[1];							\
									\
  /* Handle the special cases of NAN and infinity.  */			\
  if (__builtin_isnan (__high) || __builtin_isinf (__high))		\
    RESULT = (__float128) __high;					\
									\
  /* If low is 0.0, there no need to do the add.  In addition,		\
     avoiding the add produces the correct sign if high is -0.0.  */	\
  else if (__low == 0.0)						\
    RESULT = (__float128) __high;					\
									\
  else									\
    RESULT = ((__float128) __high) + ((__float128) __low);		\
}

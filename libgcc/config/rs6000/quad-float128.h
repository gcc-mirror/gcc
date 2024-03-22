/* Software floating-point emulation.
   Definitions for IEEE Quad Precision on the PowerPC.
   Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

/* Override the types for IEEE 128-bit scalar and complex.  We need to use the
   same IEEE 128-bit type (either long double or _Float128) for both the
   128-bit scalar type and for the base type in _Complex.  Otherwise, if
   different types are used, there may be problems in mixing _Complex values
   with the scalar value.

   We can't use _Compelx _float128 since GCC doesn't allow it.  It only allows
   _Complex for _Float128 or for long double.

   We use _Float128 when long double is IBM double-double or 64-bit, and long
   double when long double uses the IEEE 128-bit type.  We need to do this
   because the compiler has a built-in prototype for __mulkc3 and __divkc3
   using those types.  Since we are declaring these functions here, we need to
   use the same type that the compiler uses (i.e. we can't just always use
   _Float128).

   Even though the type _Float128 and long double (when long double uses IEEE
   128-bits) both hold IEEE 128-bit values, the front ends treat these as two
   separate types.

   The extension keyword __float128 is currently problematical because it can
   either be a synonym for _Float128 (when long double is IBM) or for long
   double (when long double is IEEE).  */

#ifdef __LONG_DOUBLE_IEEE128__
#define TFtype long double
#define TCtype _Complex long double

#else
#define TFtype _Float128
#define TCtype _Complex _Float128
#endif

/* Force the use of the VSX instruction set.  */
#if defined(_ARCH_PPC) && (!defined(__VSX__) || !defined(__FLOAT128__))
#pragma GCC target ("vsx,float128")
#endif

#include <stddef.h>
#include <quad.h>

#define IBM128_TYPE	__ibm128

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
extern TFtype __powikf2_sw (TFtype, SItype_ppc);
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
#ifdef _ARCH_PPC64
extern TFtype __floattikf_sw (TItype_ppc);
#endif
extern TFtype __floatunsikf_sw (USItype_ppc);
extern TFtype __floatundikf_sw (UDItype_ppc);
#ifdef _ARCH_PPC64
extern TFtype __floatuntikf_sw (UTItype_ppc);
extern TItype_ppc __fixkfti_sw (TFtype);
extern UTItype_ppc __fixunskfti_sw (TFtype);
#endif
extern IBM128_TYPE __extendkftf2_sw (TFtype);
extern TFtype __trunctfkf2_sw (IBM128_TYPE);
extern TCtype __mulkc3_sw (TFtype, TFtype, TFtype, TFtype);
extern TCtype __divkc3_sw (TFtype, TFtype, TFtype, TFtype);

#ifdef _ARCH_PPC64
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
extern TFtype __powikf2_hw (TFtype, SItype_ppc);
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
#ifdef _ARCH_PPC64
extern TFtype __floattikf_hw (TItype_ppc);
#endif
extern TFtype __floatunsikf_hw (USItype_ppc);
extern TFtype __floatundikf_hw (UDItype_ppc);
#ifdef _ARCH_PPC64
extern TFtype __floatuntikf_hw (UTItype_ppc);
extern TItype_ppc __fixkfti_hw (TFtype);
extern UTItype_ppc __fixunskfti_hw (TFtype);
#endif
extern IBM128_TYPE __extendkftf2_hw (TFtype);
extern TFtype __trunctfkf2_hw (IBM128_TYPE);
extern TCtype __mulkc3_hw (TFtype, TFtype, TFtype, TFtype);
extern TCtype __divkc3_hw (TFtype, TFtype, TFtype, TFtype);

/* Ifunc function declarations, to automatically switch between software
   emulation and hardware support.  */
extern TFtype __addkf3 (TFtype, TFtype);
extern TFtype __subkf3 (TFtype, TFtype);
extern TFtype __mulkf3 (TFtype, TFtype);
extern TFtype __divkf3 (TFtype, TFtype);
extern TFtype __negkf2 (TFtype);
extern TFtype __powikf2 (TFtype, SItype_ppc);
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
#ifdef _ARCH_PPC64
extern TFtype __floattikf (TItype_ppc);
#endif
extern TFtype __floatunsikf (USItype_ppc);
extern TFtype __floatundikf (UDItype_ppc);
#ifdef _ARCH_PPC64
extern TFtype __floatuntikf (UTItype_ppc);
extern TItype_ppc __fixkfti (TFtype);
extern UTItype_ppc __fixunskfti (TFtype);
#endif
extern IBM128_TYPE __extendkftf2 (TFtype);
extern TFtype __trunctfkf2 (IBM128_TYPE);

/* Complex __float128 built on __float128 interfaces.  */
extern TCtype __mulkc3 (TFtype, TFtype, TFtype, TFtype);
extern TCtype __divkc3 (TFtype, TFtype, TFtype, TFtype);

/* Convert IEEE 128-bit floating point to/from string.  We explicitly use
   _Float128 instead of TFmode because _strtokf and _strfromkf must be compiled
   with long double being IBM 128.  */
extern _Float128 __strtokf (const char *, char **);
extern int __strfromkf (char *restrict, size_t, const char *restrict,
			_Float128);

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
  TFtype __value = (VALUE);						\
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
      __low = (double) (__value - (TFtype) __high);			\
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
    RESULT = (TFtype) __high;						\
									\
  /* If low is 0.0, there no need to do the add.  In addition,		\
     avoiding the add produces the correct sign if high is -0.0.  */	\
  else if (__low == 0.0)						\
    RESULT = (TFtype) __high;						\
									\
  else									\
    RESULT = ((TFtype) __high) + ((TFtype) __low);			\
}

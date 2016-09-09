/* Header file for libgcc2.c.  */
/* Copyright (C) 2000-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_LIBGCC2_H
#define GCC_LIBGCC2_H

#ifndef HIDE_EXPORTS
#pragma GCC visibility push(default)
#endif

extern int __gcc_bcmp (const unsigned char *, const unsigned char *, size_t);
extern void __clear_cache (char *, char *);
extern void __eprintf (const char *, const char *, unsigned int, const char *)
  __attribute__ ((__noreturn__));

#ifdef __LIBGCC_HAS_HF_MODE__
#define LIBGCC2_HAS_HF_MODE 1
#else
#define LIBGCC2_HAS_HF_MODE 0
#endif

#ifdef __LIBGCC_HAS_SF_MODE__
#define LIBGCC2_HAS_SF_MODE 1
#else
#define LIBGCC2_HAS_SF_MODE 0
#endif

#ifdef __LIBGCC_HAS_DF_MODE__
#define LIBGCC2_HAS_DF_MODE 1
#else
#define LIBGCC2_HAS_DF_MODE 0
#endif

#ifdef __LIBGCC_HAS_XF_MODE__
#define LIBGCC2_HAS_XF_MODE 1
#else
#define LIBGCC2_HAS_XF_MODE 0
#endif

#ifdef __LIBGCC_HAS_TF_MODE__
#define LIBGCC2_HAS_TF_MODE 1
#else
#define LIBGCC2_HAS_TF_MODE 0
#endif

#ifndef __LIBGCC_SF_MANT_DIG__
#if LIBGCC2_HAS_SF_MODE
#error __LIBGCC_SF_MANT_DIG__ not defined
#else
#define __LIBGCC_SF_MANT_DIG__ 0
#endif
#endif

#ifndef __LIBGCC_DF_MANT_DIG__
#if LIBGCC2_HAS_DF_MODE
#error __LIBGCC_DF_MANT_DIG__ not defined
#else
#define __LIBGCC_DF_MANT_DIG__ 0
#endif
#endif

#ifndef __LIBGCC_XF_MANT_DIG__
#if LIBGCC2_HAS_XF_MODE
#error __LIBGCC_XF_MANT_DIG__ not defined
#else
#define __LIBGCC_XF_MANT_DIG__ 0
#endif
#endif

#ifndef __LIBGCC_TF_MANT_DIG__
#if LIBGCC2_HAS_TF_MODE
#error __LIBGCC_TF_MANT_DIG__ not defined
#else
#define __LIBGCC_TF_MANT_DIG__ 0
#endif
#endif

/* FIXME: This #ifdef probably should be removed, ie. enable the test
   for mips too.  */
/* Don't use IBM Extended Double TFmode for TI->SF calculations.
   The conversion from long double to float suffers from double
   rounding, because we convert via double.  In other cases, going
   through the software fp routines is much slower than the fallback.  */
#ifdef __powerpc__
#define AVOID_FP_TYPE_CONVERSION(SIZE) (SIZE == 106)
#elif defined(WIDEST_HARDWARE_FP_SIZE)
#define AVOID_FP_TYPE_CONVERSION(SIZE) (SIZE > WIDEST_HARDWARE_FP_SIZE)
#else
#define AVOID_FP_TYPE_CONVERSION(SIZE) 0
#endif

/* In the first part of this file, we are interfacing to calls generated
   by the compiler itself.  These calls pass values into these routines
   which have very specific modes (rather than very specific types), and
   these compiler-generated calls also expect any return values to have
   very specific modes (rather than very specific types).  Thus, we need
   to avoid using regular C language type names in this part of the file
   because the sizes for those types can be configured to be anything.
   Instead we use the following special type names.  */

typedef		 int QItype	__attribute__ ((mode (QI)));
typedef unsigned int UQItype	__attribute__ ((mode (QI)));
typedef		 int HItype	__attribute__ ((mode (HI)));
typedef unsigned int UHItype	__attribute__ ((mode (HI)));
#if MIN_UNITS_PER_WORD > 1
/* These typedefs are usually forbidden on dsp's with UNITS_PER_WORD 1.  */
typedef 	 int SItype	__attribute__ ((mode (SI)));
typedef unsigned int USItype	__attribute__ ((mode (SI)));
#if __SIZEOF_LONG_LONG__ > 4
/* These typedefs are usually forbidden on archs with UNITS_PER_WORD 2.  */
typedef		 int DItype	__attribute__ ((mode (DI)));
typedef unsigned int UDItype	__attribute__ ((mode (DI)));
#if MIN_UNITS_PER_WORD > 4
/* These typedefs are usually forbidden on archs with UNITS_PER_WORD 4.  */
typedef		 int TItype	__attribute__ ((mode (TI)));
typedef unsigned int UTItype	__attribute__ ((mode (TI)));
#endif
#endif
#endif

#if LIBGCC2_HAS_HF_MODE
typedef		float HFtype	__attribute__ ((mode (HF)));
typedef _Complex float HCtype	__attribute__ ((mode (HC)));
#endif
#if LIBGCC2_HAS_SF_MODE
typedef 	float SFtype	__attribute__ ((mode (SF)));
typedef _Complex float SCtype	__attribute__ ((mode (SC)));
#endif
#if LIBGCC2_HAS_DF_MODE
typedef		float DFtype	__attribute__ ((mode (DF)));
typedef _Complex float DCtype	__attribute__ ((mode (DC)));
#endif
#if LIBGCC2_HAS_XF_MODE
typedef		float XFtype	__attribute__ ((mode (XF)));
typedef _Complex float XCtype	__attribute__ ((mode (XC)));
#endif
#if LIBGCC2_HAS_TF_MODE
typedef		float TFtype	__attribute__ ((mode (TF)));
typedef _Complex float TCtype	__attribute__ ((mode (TC)));
#endif

typedef int cmp_return_type __attribute__((mode (__libgcc_cmp_return__)));
typedef int shift_count_type __attribute__((mode (__libgcc_shift_count__)));

/* Make sure that we don't accidentally use any normal C language built-in
   type names in the first part of this file.  Instead we want to use *only*
   the type names defined above.  The following macro definitions insure
   that if we *do* accidentally use some normal C language built-in type name,
   we will get a syntax error.  */

#define char bogus_type
#define short bogus_type
#define int bogus_type
#define long bogus_type
#define unsigned bogus_type
#define float bogus_type
#define double bogus_type

/* Versions prior to 3.4.4 were not taking into account the word size for
   the 5 trapping arithmetic functions absv, addv, subv, mulv and negv.  As
   a consequence, the si and di variants were always and the only ones emitted.
   To maintain backward compatibility, COMPAT_SIMODE_TRAPPING_ARITHMETIC is
   defined on platforms where it makes sense to still have the si variants
   emitted.  As a bonus, their implementation is now correct.  Note that the
   same mechanism should have been implemented for the di variants, but it
   turns out that no platform would define COMPAT_DIMODE_TRAPPING_ARITHMETIC
   if it existed.  */

#if LIBGCC2_UNITS_PER_WORD == 8
#define W_TYPE_SIZE (8 * __CHAR_BIT__)
#define Wtype	DItype
#define UWtype	UDItype
#define HWtype	DItype
#define UHWtype	UDItype
#define DWtype	TItype
#define UDWtype	UTItype
#ifdef LIBGCC2_GNU_PREFIX
#define __NW(a,b)	__gnu_ ## a ## di ## b
#define __NDW(a,b)	__gnu_ ## a ## ti ## b
#else
#define __NW(a,b)	__ ## a ## di ## b
#define __NDW(a,b)	__ ## a ## ti ## b
#endif
#define COMPAT_SIMODE_TRAPPING_ARITHMETIC
#elif LIBGCC2_UNITS_PER_WORD == 4
#define W_TYPE_SIZE (4 * __CHAR_BIT__)
#define Wtype	SItype
#define UWtype	USItype
#define HWtype	SItype
#define UHWtype	USItype
#define DWtype	DItype
#define UDWtype	UDItype
#ifdef LIBGCC2_GNU_PREFIX
#define __NW(a,b)	__gnu_ ## a ## si ## b
#define __NDW(a,b)	__gnu_ ## a ## di ## b
#else
#define __NW(a,b)	__ ## a ## si ## b
#define __NDW(a,b)	__ ## a ## di ## b
#endif
#elif LIBGCC2_UNITS_PER_WORD == 2
#define W_TYPE_SIZE (2 * __CHAR_BIT__)
#define Wtype	HItype
#define UWtype	UHItype
#define HWtype	HItype
#define UHWtype	UHItype
#define DWtype	SItype
#define UDWtype	USItype
#ifdef LIBGCC2_GNU_PREFIX
#define __NW(a,b)	__gnu_ ## a ## hi ## b
#define __NDW(a,b)	__gnu_ ## a ## si ## b
#else
#define __NW(a,b)	__ ## a ## hi ## b
#define __NDW(a,b)	__ ## a ## si ## b
#endif
#else
#define W_TYPE_SIZE __CHAR_BIT__
#define Wtype	QItype
#define UWtype  UQItype
#define HWtype	QItype
#define UHWtype	UQItype
#define DWtype	HItype
#define UDWtype	UHItype
#ifdef LIBGCC2_GNU_PREFIX
#define __NW(a,b)	__gnu_ ## a ## qi ## b
#define __NDW(a,b)	__gnu_ ## a ## hi ## b
#else
#define __NW(a,b)	__ ## a ## qi ## b
#define __NDW(a,b)	__ ## a ## hi ## b
#endif
#endif

#ifdef LIBGCC2_GNU_PREFIX
#define __N(a)	__gnu_ ## a
#else
#define __N(a)	__ ## a
#endif
#define Wtype_MAX ((Wtype)(((UWtype)1 << (W_TYPE_SIZE - 1)) - 1))
#define Wtype_MIN (- Wtype_MAX - 1)

#if W_TYPE_SIZE == 8
# define Wtype_MAXp1_F	0x1p8f
#elif W_TYPE_SIZE == 16
# define Wtype_MAXp1_F	0x1p16f
#elif W_TYPE_SIZE == 32
# define Wtype_MAXp1_F	0x1p32f
#elif W_TYPE_SIZE == 64
# define Wtype_MAXp1_F	0x1p64f
#else
# error "expand the table"
#endif

#define __muldi3	__NDW(mul,3)
#define __divdi3	__NDW(div,3)
#define __udivdi3	__NDW(udiv,3)
#define __moddi3	__NDW(mod,3)
#define __umoddi3	__NDW(umod,3)
#define __negdi2	__NDW(neg,2)
#define __lshrdi3	__NDW(lshr,3)
#define __ashldi3	__NDW(ashl,3)
#define __ashrdi3	__NDW(ashr,3)
#define __cmpdi2	__NDW(cmp,2)
#define __ucmpdi2	__NDW(ucmp,2)
#define __udivmoddi4	__NDW(udivmod,4)
#define __fixunstfDI	__NDW(fixunstf,)
#define __fixtfdi	__NDW(fixtf,)
#define __fixunsxfDI	__NDW(fixunsxf,)
#define __fixxfdi	__NDW(fixxf,)
#define __fixunsdfDI	__NDW(fixunsdf,)
#define __fixdfdi	__NDW(fixdf,)
#define __fixunssfDI	__NDW(fixunssf,)
#define __fixsfdi	__NDW(fixsf,)
#define __floatdixf	__NDW(float,xf)
#define __floatditf	__NDW(float,tf)
#define __floatdidf	__NDW(float,df)
#define __floatdisf	__NDW(float,sf)
#define __floatundixf	__NDW(floatun,xf)
#define __floatunditf	__NDW(floatun,tf)
#define __floatundidf	__NDW(floatun,df)
#define __floatundisf	__NDW(floatun,sf)
#define __fixunsxfSI	__NW(fixunsxf,)
#define __fixunstfSI	__NW(fixunstf,)
#define __fixunsdfSI	__NW(fixunsdf,)
#define __fixunssfSI	__NW(fixunssf,)

#define __absvSI2	__NW(absv,2)
#define __addvSI3	__NW(addv,3)
#define __subvSI3	__NW(subv,3)
#define __mulvSI3	__NW(mulv,3)
#define __negvSI2	__NW(negv,2)
#define __absvDI2	__NDW(absv,2)
#define __addvDI3	__NDW(addv,3)
#define __subvDI3	__NDW(subv,3)
#define __mulvDI3	__NDW(mulv,3)
#define __negvDI2	__NDW(negv,2)

#define __ffsSI2	__NW(ffs,2)
#define __clzSI2	__NW(clz,2)
#define __ctzSI2	__NW(ctz,2)
#define __clrsbSI2	__NW(clrsb,2)
#define __popcountSI2	__NW(popcount,2)
#define __paritySI2	__NW(parity,2)
#define __ffsDI2	__NDW(ffs,2)
#define __clzDI2	__NDW(clz,2)
#define __ctzDI2	__NDW(ctz,2)
#define __clrsbDI2	__NDW(clrsb,2)
#define __popcountDI2	__NDW(popcount,2)
#define __parityDI2	__NDW(parity,2)

#define __clz_tab		__N(clz_tab)
#define __bswapsi2		__N(bswapsi2)
#define __bswapdi2		__N(bswapdi2)
#define __udiv_w_sdiv		__N(udiv_w_sdiv)
#define __clear_cache		__N(clear_cache)
#define __enable_execute_stack	__N(enable_execute_stack)

#ifndef __powisf2
#define __powisf2		__N(powisf2)
#endif
#ifndef __powidf2
#define __powidf2		__N(powidf2)
#endif
#ifndef __powitf2
#define __powitf2		__N(powitf2)
#endif
#ifndef __powixf2
#define __powixf2		__N(powixf2)
#endif
#ifndef __mulsc3
#define __mulsc3		__N(mulsc3)
#endif
#ifndef __muldc3
#define __muldc3		__N(muldc3)
#endif
#ifndef __mulxc3
#define __mulxc3		__N(mulxc3)
#endif
#ifndef __multc3
#define __multc3		__N(multc3)
#endif
#ifndef __divsc3
#define __divsc3		__N(divsc3)
#endif
#ifndef __divdc3
#define __divdc3		__N(divdc3)
#endif
#ifndef __divxc3
#define __divxc3		__N(divxc3)
#endif
#ifndef __divtc3
#define __divtc3		__N(divtc3)
#endif

extern DWtype __muldi3 (DWtype, DWtype);
extern DWtype __divdi3 (DWtype, DWtype);
extern UDWtype __udivdi3 (UDWtype, UDWtype);
extern UDWtype __umoddi3 (UDWtype, UDWtype);
extern DWtype __moddi3 (DWtype, DWtype);

/* __udivmoddi4 is static inline when building other libgcc2 portions.  */
#if (!defined (L_udivdi3) && !defined (L_divdi3) && \
     !defined (L_umoddi3) && !defined (L_moddi3))
extern UDWtype __udivmoddi4 (UDWtype, UDWtype, UDWtype *);
#endif

/* __negdi2 is static inline when building other libgcc2 portions.  */
#if !defined(L_divdi3) && !defined(L_moddi3)
extern DWtype __negdi2 (DWtype);
#endif

extern DWtype __lshrdi3 (DWtype, shift_count_type);
extern DWtype __ashldi3 (DWtype, shift_count_type);
extern DWtype __ashrdi3 (DWtype, shift_count_type);

/* __udiv_w_sdiv is static inline when building other libgcc2 portions.  */
#if (!defined(L_udivdi3) && !defined(L_divdi3) && \
     !defined(L_umoddi3) && !defined(L_moddi3))
extern UWtype __udiv_w_sdiv (UWtype *, UWtype, UWtype, UWtype);
#endif

extern cmp_return_type __cmpdi2 (DWtype, DWtype);
extern cmp_return_type __ucmpdi2 (DWtype, DWtype);

#if MIN_UNITS_PER_WORD > 1
extern SItype __bswapsi2 (SItype);
#endif
#if __SIZEOF_LONG_LONG__ > 4
extern DItype __bswapdi2 (DItype);
#endif

extern Wtype __absvSI2 (Wtype);
extern Wtype __addvSI3 (Wtype, Wtype);
extern Wtype __subvSI3 (Wtype, Wtype);
extern Wtype __mulvSI3 (Wtype, Wtype);
extern Wtype __negvSI2 (Wtype);
extern DWtype __absvDI2 (DWtype);
extern DWtype __addvDI3 (DWtype, DWtype);
extern DWtype __subvDI3 (DWtype, DWtype);
extern DWtype __mulvDI3 (DWtype, DWtype);
extern DWtype __negvDI2 (DWtype);

#ifdef COMPAT_SIMODE_TRAPPING_ARITHMETIC
#define __absvsi2	__N(absvsi2)
#define __negvsi2	__N(negvsi2)
#define __addvsi3	__N(addvsi3)
#define __subvsi3	__N(subvsi3)
#define __mulvsi3	__N(mulvsi3)

extern SItype __absvsi2 (SItype);
extern SItype __addvsi3 (SItype, SItype);
extern SItype __subvsi3 (SItype, SItype);
extern SItype __mulvsi3 (SItype, SItype);
extern SItype __negvsi2 (SItype);
#endif /* COMPAT_SIMODE_TRAPPING_ARITHMETIC */

#undef int
#if LIBGCC2_HAS_HF_MODE
extern HCtype __divhc3 (HFtype, HFtype, HFtype, HFtype);
extern HCtype __mulhc3 (HFtype, HFtype, HFtype, HFtype);
#endif
#if LIBGCC2_HAS_SF_MODE
extern DWtype __fixsfdi (SFtype);
extern SFtype __floatdisf (DWtype);
extern SFtype __floatundisf (UDWtype);
extern UWtype __fixunssfSI (SFtype);
extern UDWtype __fixunssfDI (SFtype);
extern SFtype __powisf2 (SFtype, int);
extern SCtype __divsc3 (SFtype, SFtype, SFtype, SFtype);
extern SCtype __mulsc3 (SFtype, SFtype, SFtype, SFtype);
#endif
#if LIBGCC2_HAS_DF_MODE
extern DWtype __fixdfdi (DFtype);
extern DFtype __floatdidf (DWtype);
extern DFtype __floatundidf (UDWtype);
extern UWtype __fixunsdfSI (DFtype);
extern UDWtype __fixunsdfDI (DFtype);
extern DFtype __powidf2 (DFtype, int);
extern DCtype __divdc3 (DFtype, DFtype, DFtype, DFtype);
extern DCtype __muldc3 (DFtype, DFtype, DFtype, DFtype);
#endif

#if LIBGCC2_HAS_XF_MODE
extern DWtype __fixxfdi (XFtype);
extern UDWtype __fixunsxfDI (XFtype);
extern XFtype __floatdixf (DWtype);
extern XFtype __floatundixf (UDWtype);
extern UWtype __fixunsxfSI (XFtype);
extern XFtype __powixf2 (XFtype, int);
extern XCtype __divxc3 (XFtype, XFtype, XFtype, XFtype);
extern XCtype __mulxc3 (XFtype, XFtype, XFtype, XFtype);
#endif

#if LIBGCC2_HAS_TF_MODE
extern UDWtype __fixunstfDI (TFtype);
extern DWtype __fixtfdi (TFtype);
extern TFtype __floatditf (DWtype);
extern TFtype __floatunditf (UDWtype);
extern TFtype __powitf2 (TFtype, int);
extern TCtype __divtc3 (TFtype, TFtype, TFtype, TFtype);
extern TCtype __multc3 (TFtype, TFtype, TFtype, TFtype);
#endif
#define int bogus_type

/* DWstructs are pairs of Wtype values in the order determined by
   __BYTE_ORDER__.  */

#if __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
  struct DWstruct {Wtype high, low;};
#else
  struct DWstruct {Wtype low, high;};
#endif

/* We need this union to unpack/pack DImode values, since we don't have
   any arithmetic yet.  Incoming DImode parameters are stored into the
   `ll' field, and the unpacked result is read from the struct `s'.  */

typedef union
{
  struct DWstruct s;
  DWtype ll;
} DWunion;

/* Defined for L_popcount_tab.  Exported here because some targets may
   want to use it for their own versions of the __popcount builtins.  */
extern const UQItype __popcount_tab[256];

/* Defined for L_clz.  Exported here because some targets may want to use
   it for their own versions of the __clz builtins.  It contains the bit
   position of the first set bit for the numbers 0 - 255.  This avoids the
   need for a separate table for the __ctz builtins.  */
extern const UQItype __clz_tab[256];

#include "longlong.h"

#undef int
extern int __clzDI2 (UDWtype);
extern int __clzSI2 (UWtype);
extern int __ctzSI2 (UWtype);
extern int __ctzDI2 (UDWtype);
extern int __clrsbSI2 (Wtype);
extern int __clrsbDI2 (DWtype);
extern int __ffsSI2 (UWtype);
extern int __ffsDI2 (DWtype);
extern int __popcountSI2 (UWtype);
extern int __popcountDI2 (UDWtype);
extern int __paritySI2 (UWtype);
extern int __parityDI2 (UDWtype);
#define int bogus_type

extern void __enable_execute_stack (void *);

#ifndef HIDE_EXPORTS
#pragma GCC visibility pop
#endif

#endif /* ! GCC_LIBGCC2_H */

/* Copyright (C) 2007-2021 Free Software Foundation, Inc.

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

#ifndef _BID_GCC_INTRINSICS_H
#define _BID_GCC_INTRINSICS_H

#ifdef IN_LIBGCC2

#include "tconfig.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"

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

#ifndef BID_HAS_XF_MODE
#define BID_HAS_XF_MODE LIBGCC2_HAS_XF_MODE
#endif

#ifndef BID_HAS_TF_MODE
#define BID_HAS_TF_MODE LIBGCC2_HAS_TF_MODE
#endif

/* Some handy typedefs.  */

typedef float SFtype __attribute__ ((mode (SF)));
typedef float DFtype __attribute__ ((mode (DF)));
#if LIBGCC2_HAS_XF_MODE
typedef float XFtype __attribute__ ((mode (XF)));
#endif /* LIBGCC2_HAS_XF_MODE */
#if LIBGCC2_HAS_TF_MODE
typedef float TFtype __attribute__ ((mode (TF)));
#endif /* LIBGCC2_HAS_XF_MODE */

typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));

/* The type of the result of a decimal float comparison.  This must
   match `word_mode' in GCC for the target.  */

typedef int CMPtype __attribute__ ((mode (word)));

typedef int SINT8 __attribute__ ((mode (QI)));
typedef unsigned int UINT8 __attribute__ ((mode (QI)));
typedef USItype UINT32;
typedef SItype SINT32;
typedef UDItype UINT64;
typedef DItype SINT64;

/* It has to be identical to the one defined in bid_functions.h.  */
typedef __attribute__ ((aligned(16))) struct
{
  UINT64 w[2];
} UINT128;
#else	/* if not IN_LIBGCC2 */

#ifndef BID_HAS_XF_MODE
#define BID_HAS_XF_MODE 1
#endif

#ifndef BID_HAS_TF_MODE
#if defined __i386__
#define BID_HAS_TF_MODE 0
#else
#define BID_HAS_TF_MODE 1
#endif
#endif

#ifndef SFtype
#define SFtype float
#endif

#ifndef DFtype
#define DFtype double
#endif

#if BID_HAS_XF_MODE
#ifndef XFtype
#define XFtype long double
#endif

#endif   /* IN_LIBGCC2 */

#if BID_HAS_TF_MODE
#ifndef TFtype
#define TFtype __float128
#endif
#endif

#ifndef SItype
#define SItype SINT32
#endif

#ifndef DItype
#define DItype SINT64
#endif

#ifndef USItype
#define USItype UINT32
#endif

#ifndef UDItype
#define UDItype UINT64
#endif

#ifndef CMPtype
#define CMPtype long
#endif
#endif	/* IN_LIBGCC2 */

#if BID_HAS_GCC_DECIMAL_INTRINSICS
/* Prototypes for gcc instrinsics  */

extern _Decimal64 __bid_adddd3 (_Decimal64, _Decimal64);
extern _Decimal64 __bid_subdd3 (_Decimal64, _Decimal64);
extern _Decimal32 __bid_addsd3 (_Decimal32, _Decimal32);
extern _Decimal32 __bid_subsd3 (_Decimal32, _Decimal32);
extern _Decimal128 __bid_addtd3 (_Decimal128, _Decimal128);
extern _Decimal128 __bid_subtd3 (_Decimal128, _Decimal128);
extern DFtype __bid_truncdddf (_Decimal64);
extern DItype __bid_fixdddi (_Decimal64);
extern _Decimal32 __bid_truncddsd2 (_Decimal64);
extern SFtype __bid_truncddsf (_Decimal64);
extern SItype __bid_fixddsi (_Decimal64);
extern _Decimal128 __bid_extendddtd2 (_Decimal64);
#if BID_HAS_TF_MODE
extern TFtype __bid_extendddtf (_Decimal64);
#endif
extern UDItype __bid_fixunsdddi (_Decimal64);
extern USItype __bid_fixunsddsi (_Decimal64);
#if BID_HAS_XF_MODE
extern XFtype __bid_extendddxf (_Decimal64);
#endif
extern _Decimal64 __bid_extenddfdd (DFtype);
extern _Decimal32 __bid_truncdfsd (DFtype);
extern _Decimal128 __bid_extenddftd (DFtype);
extern _Decimal64 __bid_floatdidd (DItype);
extern _Decimal32 __bid_floatdisd (DItype);
extern _Decimal128 __bid_floatditd (DItype);
extern _Decimal64 __bid_divdd3 (_Decimal64, _Decimal64);
extern _Decimal32 __bid_divsd3 (_Decimal32, _Decimal32);
extern _Decimal128 __bid_divtd3 (_Decimal128, _Decimal128);
extern CMPtype __bid_eqdd2 (_Decimal64, _Decimal64);
extern CMPtype __bid_eqsd2 (_Decimal32, _Decimal32);
extern CMPtype __bid_eqtd2 (_Decimal128, _Decimal128);
extern CMPtype __bid_gedd2 (_Decimal64, _Decimal64);
extern CMPtype __bid_gesd2 (_Decimal32, _Decimal32);
extern CMPtype __bid_getd2 (_Decimal128, _Decimal128);
extern CMPtype __bid_gtdd2 (_Decimal64, _Decimal64);
extern CMPtype __bid_gtsd2 (_Decimal32, _Decimal32);
extern CMPtype __bid_gttd2 (_Decimal128, _Decimal128);
extern CMPtype __bid_ledd2 (_Decimal64, _Decimal64);
extern CMPtype __bid_lesd2 (_Decimal32, _Decimal32);
extern CMPtype __bid_letd2 (_Decimal128, _Decimal128);
extern CMPtype __bid_ltdd2 (_Decimal64, _Decimal64);
extern CMPtype __bid_ltsd2 (_Decimal32, _Decimal32);
extern CMPtype __bid_lttd2 (_Decimal128, _Decimal128);
extern CMPtype __bid_nedd2 (_Decimal64, _Decimal64);
extern CMPtype __bid_nesd2 (_Decimal32, _Decimal32);
extern CMPtype __bid_netd2 (_Decimal128, _Decimal128);
extern CMPtype __bid_unorddd2 (_Decimal64, _Decimal64);
extern CMPtype __bid_unordsd2 (_Decimal32, _Decimal32);
extern CMPtype __bid_unordtd2 (_Decimal128, _Decimal128);
extern _Decimal64 __bid_muldd3 (_Decimal64, _Decimal64);
extern _Decimal32 __bid_mulsd3 (_Decimal32, _Decimal32);
extern _Decimal128 __bid_multd3 (_Decimal128, _Decimal128);
extern _Decimal64 __bid_extendsddd2 (_Decimal32);
extern DFtype __bid_extendsddf (_Decimal32);
extern DItype __bid_fixsddi (_Decimal32);
extern SFtype __bid_truncsdsf (_Decimal32);
extern SItype __bid_fixsdsi (_Decimal32);
extern _Decimal128 __bid_extendsdtd2 (_Decimal32);
#if BID_HAS_TF_MODE
extern TFtype __bid_extendsdtf (_Decimal32);
#endif
extern UDItype __bid_fixunssddi (_Decimal32);
extern USItype __bid_fixunssdsi (_Decimal32);
#if BID_HAS_XF_MODE
extern XFtype __bid_extendsdxf (_Decimal32);
#endif
extern _Decimal64 __bid_extendsfdd (SFtype);
extern _Decimal32 __bid_extendsfsd (SFtype);
extern _Decimal128 __bid_extendsftd (SFtype);
extern _Decimal64 __bid_floatsidd (SItype);
extern _Decimal32 __bid_floatsisd (SItype);
extern _Decimal128 __bid_floatsitd (SItype);
extern _Decimal64 __bid_trunctddd2 (_Decimal128);
extern DFtype __bid_trunctddf (_Decimal128);
extern DItype __bid_fixtddi (_Decimal128);
extern _Decimal32 __bid_trunctdsd2 (_Decimal128);
extern SFtype __bid_trunctdsf (_Decimal128);
extern SItype __bid_fixtdsi (_Decimal128);
#if BID_HAS_TF_MODE
extern TFtype __bid_trunctdtf (_Decimal128);
#endif
extern UDItype __bid_fixunstddi (_Decimal128);
extern USItype __bid_fixunstdsi (_Decimal128);
#if BID_HAS_XF_MODE
extern XFtype __bid_trunctdxf (_Decimal128);
#endif
#if BID_HAS_TF_MODE
extern _Decimal64 __bid_trunctfdd (TFtype);
extern _Decimal32 __bid_trunctfsd (TFtype);
extern _Decimal128 __bid_extendtftd (TFtype);
#endif
extern _Decimal64 __bid_floatunsdidd (UDItype);
extern _Decimal32 __bid_floatunsdisd (UDItype);
extern _Decimal128 __bid_floatunsditd (UDItype);
extern _Decimal64 __bid_floatunssidd (USItype);
extern _Decimal32 __bid_floatunssisd (USItype);
extern _Decimal128 __bid_floatunssitd (USItype);
#if BID_HAS_XF_MODE
extern _Decimal64 __bid_truncxfdd (XFtype);
extern _Decimal32 __bid_truncxfsd (XFtype);
extern _Decimal128 __bid_extendxftd (XFtype);
#endif
extern int isinfd32 (_Decimal32);
extern int isinfd64 (_Decimal64);
extern int isinfd128 (_Decimal128);
#endif  /* BID_HAS_GCC_DECIMAL_INTRINSICS */

extern void __dfp_set_round (int);
extern int __dfp_get_round (void);
extern void __dfp_clear_except (void);
extern int __dfp_test_except (int);
extern void __dfp_raise_except (int);

#if BID_HAS_GCC_DECIMAL_INTRINSICS
/* Used by gcc intrinsics.  We have to define them after UINT128
   is defined.  */
union decimal32 {
  _Decimal32 d;
  UINT32 i;
};
 
union decimal64 {
  _Decimal64 d;
  UINT64 i;
};
 
union decimal128 {
  _Decimal128 d;
  UINT128 i;
};
 
#if BID_HAS_TF_MODE
union float128 {
  TFtype f;
  UINT128 i;
};
#endif
#endif  /* BID_HAS_GCC_DECIMAL_INTRINSICS */

#endif /* _BID_GCC_INTRINSICS_H */

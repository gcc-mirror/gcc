/* This is a software fixed-point library.
   Copyright (C) 2007-2025 Free Software Foundation, Inc.

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

#ifndef _FIXED_BIT_H
#define _FIXED_BIT_H

#ifdef LIBGCC2_GNU_PREFIX
#define LIBGCC2_FIXEDBIT_GNU_PREFIX
#endif

/* To use this file we need to define one of the following:
   QQ_MODE, UQQ_MODE, HQ_MODE, UHQ_MODE, SQ_MODE, USQ_MODE, DQ_MODE, UDQ_MODE,
   TQ_MODE, UTQ_MODE, HA_MODE, UHA_MODE, SA_MODE, USA_MODE, DA_MODE, UDA_MODE,
   TA_MODE, UTA_MODE.
   Then, all operators for this machine mode will be created.

   Or, we need to define FROM_* TO_* for conversions from one mode to another
   mode.  The mode could be one of the following:
   Fract: QQ, UQQ, HQ, UHQ, SQ, USQ, DQ, UDQ, TQ, UTQ
   Accum: HA, UHA, SA, USA, DA, UDA, TA, UTA
   Signed integer: QI, HI, SI, DI, TI
   Unsigned integer: UQI, UHI, USI, UDI, UTI
   Floating-point: SF, DF
   Ex: If we define FROM_QQ and TO_SI, the conversion from QQ to SI is
   generated.  */

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

typedef          int QItype     __attribute__ ((mode (QI)));
typedef unsigned int UQItype    __attribute__ ((mode (QI)));
typedef          int HItype     __attribute__ ((mode (HI)));
typedef unsigned int UHItype    __attribute__ ((mode (HI)));
typedef          _Fract QQtype  __attribute__ ((mode (QQ)));
typedef unsigned _Fract UQQtype __attribute__ ((mode (UQQ)));
typedef          _Fract HQtype  __attribute__ ((mode (HQ)));
typedef unsigned _Fract UHQtype __attribute__ ((mode (UHQ)));
typedef          _Fract HAtype  __attribute__ ((mode (HA)));
typedef unsigned _Fract UHAtype __attribute__ ((mode (UHA)));
#define HAVE_QQ		1
#define HAVE_UQQ	1
#define HAVE_HQ		1
#define HAVE_UHQ	1
#define HAVE_HA		1
#define HAVE_UHA	1
#define HAVE_QI		1
#define HAVE_UQI	1
#define HAVE_HI		1
#define HAVE_UHI	1
#if MIN_UNITS_PER_WORD > 1
/* These typedefs are usually forbidden on dsp's with UNITS_PER_WORD 1.  */
typedef          int SItype     __attribute__ ((mode (SI)));
typedef unsigned int USItype    __attribute__ ((mode (SI)));
typedef          _Fract SQtype  __attribute__ ((mode (SQ)));
typedef unsigned _Fract USQtype __attribute__ ((mode (USQ)));
typedef          _Fract SAtype  __attribute__ ((mode (SA)));
typedef unsigned _Fract USAtype __attribute__ ((mode (USA)));
#define HAVE_SQ		1
#define HAVE_USQ	1
#define HAVE_SA		1
#define HAVE_USA	1
#define HAVE_SI		1
#define HAVE_USI	1
#if LONG_LONG_TYPE_SIZE > 32
/* These typedefs are usually forbidden on archs with UNITS_PER_WORD 2.  */
typedef          int DItype     __attribute__ ((mode (DI)));
typedef unsigned int UDItype    __attribute__ ((mode (DI)));
typedef          _Fract DQtype  __attribute__ ((mode (DQ)));
typedef unsigned _Fract UDQtype __attribute__ ((mode (UDQ)));
typedef          _Fract DAtype  __attribute__ ((mode (DA)));
typedef unsigned _Fract UDAtype __attribute__ ((mode (UDA)));
#define HAVE_DQ		1
#define HAVE_UDQ	1
#define HAVE_DA		1
#define HAVE_UDA	1
#define HAVE_DI		1
#define HAVE_UDI	1
#if MIN_UNITS_PER_WORD > 4
/* These typedefs are usually forbidden on archs with UNITS_PER_WORD 4.  */
typedef          int TItype     __attribute__ ((mode (TI)));
typedef unsigned int UTItype    __attribute__ ((mode (TI)));
typedef          _Fract TQtype  __attribute__ ((mode (TQ)));
typedef unsigned _Fract UTQtype __attribute__ ((mode (UTQ)));
typedef          _Fract TAtype  __attribute__ ((mode (TA)));
typedef unsigned _Fract UTAtype __attribute__ ((mode (UTA)));
#define HAVE_TQ		1
#define HAVE_UTQ	1
#define HAVE_TA		1
#define HAVE_UTA	1
#define HAVE_TI		1
#define HAVE_UTI	1
#endif
#endif
#endif

#if LIBGCC2_HAS_SF_MODE
typedef float SFtype __attribute__ ((mode (SF)));
#define HAVE_SF		1
#endif
#if LIBGCC2_HAS_DF_MODE
typedef float DFtype __attribute__ ((mode (DF)));
#define HAVE_DF		1
#endif

typedef int word_type __attribute__ ((mode (__word__)));

/* Based on modes, we create many defines.  */

#if defined (QQ_MODE) && (HAVE_QQ == 1)
#define FIXED_SIZE	1	/* in bytes.  */
#define INT_C_TYPE	QItype
#define UINT_C_TYPE	UQItype
#define DINT_C_TYPE	HItype
#define DUINT_C_TYPE	UHItype
#define MODE_NAME	QQ
#define MODE_NAME_S	qq
#define MODE_UNSIGNED	0
#endif

#if defined (UQQ_MODE) && (HAVE_UQQ == 1)
#define FIXED_SIZE	1	/* in bytes.  */
#define INT_C_TYPE	UQItype
#define UINT_C_TYPE	UQItype
#define DINT_C_TYPE	UHItype
#define DUINT_C_TYPE	UHItype
#define MODE_NAME	UQQ
#define MODE_NAME_S	uqq
#define MODE_UNSIGNED	1
#endif

#if defined (HQ_MODE) && (HAVE_HQ == 1)
#define FIXED_SIZE	2	/* in bytes.  */
#define INT_C_TYPE	HItype
#define UINT_C_TYPE	UHItype

#if HAVE_SI == 1
#define DINT_C_TYPE	SItype
#define DUINT_C_TYPE	USItype
#else
#define HINT_C_TYPE	QItype
#define HUINT_C_TYPE	UQItype
#endif

#define MODE_NAME	HQ
#define MODE_NAME_S	hq
#define MODE_UNSIGNED	0
#endif

#if defined (UHQ_MODE) && (HAVE_UHQ == 1)
#define FIXED_SIZE	2	/* in bytes.  */
#define INT_C_TYPE	UHItype
#define UINT_C_TYPE	UHItype

#if HAVE_SI == 1
#define DINT_C_TYPE	USItype
#define DUINT_C_TYPE	USItype
#else
#define HINT_C_TYPE	UQItype
#define HUINT_C_TYPE	UQItype
#endif

#define MODE_NAME	UHQ
#define MODE_NAME_S	uhq
#define MODE_UNSIGNED	1
#endif

#if defined (SQ_MODE) && (HAVE_SQ == 1)
#define FIXED_SIZE	4	/* in bytes.  */
#define INT_C_TYPE	SItype
#define UINT_C_TYPE	USItype

#if HAVE_DI == 1
#define DINT_C_TYPE	DItype
#define DUINT_C_TYPE	UDItype
#else
#define HINT_C_TYPE	HItype
#define HUINT_C_TYPE	UHItype
#endif

#define MODE_NAME	SQ
#define MODE_NAME_S	sq
#define MODE_UNSIGNED	0
#endif

#if defined (USQ_MODE) && (HAVE_USQ == 1)
#define FIXED_SIZE	4	/* in bytes.  */
#define INT_C_TYPE	USItype
#define UINT_C_TYPE	USItype

#if HAVE_DI == 1
#define DINT_C_TYPE	UDItype
#define DUINT_C_TYPE	UDItype
#else
#define HINT_C_TYPE	UHItype
#define HUINT_C_TYPE	UHItype
#endif

#define MODE_NAME	USQ
#define MODE_NAME_S	usq
#define MODE_UNSIGNED	1
#endif

#if defined (DQ_MODE) && (HAVE_DQ == 1)
#define FIXED_SIZE	8	/* in bytes.  */
#define INT_C_TYPE	DItype
#define UINT_C_TYPE	UDItype

#if HAVE_TI == 1
#define DINT_C_TYPE	TItype
#define DUINT_C_TYPE	UTItype
#else
#define HINT_C_TYPE	SItype
#define HUINT_C_TYPE	USItype
#endif

#define MODE_NAME	DQ
#define MODE_NAME_S	dq
#define MODE_UNSIGNED	0
#endif

#if defined (UDQ_MODE) && (HAVE_UDQ == 1)
#define FIXED_SIZE	8	/* in bytes.  */
#define INT_C_TYPE	UDItype
#define UINT_C_TYPE	UDItype

#if HAVE_TI == 1
#define DINT_C_TYPE	UTItype
#define DUINT_C_TYPE	UTItype
#else
#define HINT_C_TYPE	USItype
#define HUINT_C_TYPE	USItype
#endif

#define MODE_NAME	UDQ
#define MODE_NAME_S	udq
#define MODE_UNSIGNED	1
#endif

#if defined (TQ_MODE) && (HAVE_TQ == 1)
#define FIXED_SIZE	16	/* in bytes.  */
#define INT_C_TYPE	TItype
#define UINT_C_TYPE	UTItype
#define HINT_C_TYPE	DItype
#define HUINT_C_TYPE	UDItype
#define MODE_NAME	TQ
#define MODE_NAME_S	tq
#define MODE_UNSIGNED	0
#endif

#if defined (UTQ_MODE) && (HAVE_UTQ == 1)
#define FIXED_SIZE	16	/* in bytes.  */
#define INT_C_TYPE	UTItype
#define UINT_C_TYPE	UTItype
#define HINT_C_TYPE	UDItype
#define HUINT_C_TYPE	UDItype
#define MODE_NAME	UTQ
#define MODE_NAME_S	utq
#define MODE_UNSIGNED	1
#endif

#if defined (HA_MODE) && (HAVE_HA == 1)
#define FIXED_SIZE	2	/* in bytes.  */
#define INT_C_TYPE	HItype
#define UINT_C_TYPE	UHItype

#if HAVE_SI == 1
#define DINT_C_TYPE	SItype
#define DUINT_C_TYPE	USItype
#else
#define HINT_C_TYPE	QItype
#define HUINT_C_TYPE	UQItype
#endif

#define MODE_NAME	HA
#define MODE_NAME_S	ha
#define MODE_UNSIGNED	0
#endif

#if defined (UHA_MODE) && (HAVE_UHA == 1)
#define FIXED_SIZE	2	/* in bytes.  */
#define INT_C_TYPE	UHItype
#define UINT_C_TYPE	UHItype

#if HAVE_SI == 1
#define DINT_C_TYPE	USItype
#define DUINT_C_TYPE	USItype
#else
#define HINT_C_TYPE	UQItype
#define HUINT_C_TYPE	UQItype
#endif

#define MODE_NAME	UHA
#define MODE_NAME_S	uha
#define MODE_UNSIGNED	1
#endif

#if defined (SA_MODE) && (HAVE_SA == 1)
#define FIXED_SIZE	4	/* in bytes.  */
#define INT_C_TYPE	SItype
#define UINT_C_TYPE	USItype

#if HAVE_DI == 1
#define DINT_C_TYPE	DItype
#define DUINT_C_TYPE	UDItype
#else
#define HINT_C_TYPE	HItype
#define HUINT_C_TYPE	UHItype
#endif

#define MODE_NAME	SA
#define MODE_NAME_S	sa
#define MODE_UNSIGNED	0
#endif

#if defined (USA_MODE) && (HAVE_USA == 1)
#define FIXED_SIZE	4	/* in bytes.  */
#define INT_C_TYPE	USItype
#define UINT_C_TYPE	USItype

#if HAVE_DI == 1
#define DINT_C_TYPE	UDItype
#define DUINT_C_TYPE	UDItype
#else
#define HINT_C_TYPE	UHItype
#define HUINT_C_TYPE	UHItype
#endif

#define MODE_NAME	USA
#define MODE_NAME_S	usa
#define MODE_UNSIGNED	1
#endif

#if defined (DA_MODE) && (HAVE_DA == 1)
#define FIXED_SIZE	8	/* in bytes.  */
#define INT_C_TYPE	DItype
#define UINT_C_TYPE	UDItype

#if HAVE_TI == 1
#define DINT_C_TYPE	TItype
#define DUINT_C_TYPE	UTItype
#else
#define HINT_C_TYPE	SItype
#define HUINT_C_TYPE	USItype
#endif

#define MODE_NAME	DA
#define MODE_NAME_S	da
#define MODE_UNSIGNED	0
#endif

#if defined (UDA_MODE) && (HAVE_UDA == 1)
#define FIXED_SIZE	8	/* in bytes.  */
#define INT_C_TYPE	UDItype
#define UINT_C_TYPE	UDItype

#if HAVE_TI == 1
#define DINT_C_TYPE	UTItype
#define DUINT_C_TYPE	UTItype
#else
#define HINT_C_TYPE	USItype
#define HUINT_C_TYPE	USItype
#endif

#define MODE_NAME	UDA
#define MODE_NAME_S	uda
#define MODE_UNSIGNED	1
#endif

#if defined (TA_MODE) && (HAVE_TA == 1)
#define FIXED_SIZE	16	/* in bytes.  */
#define INT_C_TYPE	TItype
#define UINT_C_TYPE	UTItype
#define HINT_C_TYPE	DItype
#define HUINT_C_TYPE	UDItype
#define MODE_NAME	TA
#define MODE_NAME_S	ta
#define MODE_UNSIGNED	0
#endif

#if defined (UTA_MODE) && (HAVE_UTA == 1)
#define FIXED_SIZE	16	/* in bytes.  */
#define INT_C_TYPE	UTItype
#define UINT_C_TYPE	UTItype
#define HINT_C_TYPE	UDItype
#define HUINT_C_TYPE	UDItype
#define MODE_NAME	UTA
#define MODE_NAME_S	uta
#define MODE_UNSIGNED	1
#endif

/* The following defines are based on the previous defines.  */

#if defined (HINT_C_TYPE)
#if __BYTE_ORDER__ != __ORDER_LITTLE_ENDIAN__
  struct INTstruct {HINT_C_TYPE high, low;};
#else
  struct INTstruct {HINT_C_TYPE low, high;};
#endif

typedef union
{
  struct INTstruct s;
  INT_C_TYPE ll;
} INTunion;
#endif

#define FIXED_WIDTH	(FIXED_SIZE * __CHAR_BIT__) /* in bits.  */
#define FIXED_C_TYPE1(NAME)	NAME ## type
#define FIXED_C_TYPE2(NAME)	FIXED_C_TYPE1(NAME)
#define FIXED_C_TYPE	FIXED_C_TYPE2(MODE_NAME)
#define FBITS1(NAME)	__ ## NAME ## _FBIT__
#define FBITS2(NAME)	FBITS1(NAME)
#define FBITS		FBITS2(MODE_NAME)
#define IBITS1(NAME)	__ ## NAME ## _IBIT__
#define IBITS2(NAME)	IBITS1(NAME)
#define IBITS		IBITS2(MODE_NAME)
#define I_F_BITS	(FBITS + IBITS)

#ifdef LIBGCC2_FIXEDBIT_GNU_PREFIX
#define FIXED_OP(OP,MODE,NUM)	__gnu_ ## OP ## MODE ## NUM
#else
#define FIXED_OP(OP,MODE,NUM)	__ ## OP ## MODE ## NUM
#endif

#define FIXED_SATURATE1_TEMP(NAME)	FIXED_OP(saturate1,NAME,)
#define FIXED_SATURATE2_TEMP(NAME)	FIXED_OP(saturate2,NAME,)
#define FIXED_MULHELPER_TEMP(NAME)	FIXED_OP(mulhelper,NAME,)
#define FIXED_DIVHELPER_TEMP(NAME)	FIXED_OP(divhelper,NAME,)
#define FIXED_ASHLHELPER_TEMP(NAME)	FIXED_OP(ashlhelper,NAME,)
#define FIXED_ADD_TEMP(NAME)	FIXED_OP(add,NAME,3)
#define FIXED_SSADD_TEMP(NAME)	FIXED_OP(ssadd,NAME,3)
#define FIXED_USADD_TEMP(NAME)	FIXED_OP(usadd,NAME,3)
#define FIXED_SUB_TEMP(NAME)	FIXED_OP(sub,NAME,3)
#define FIXED_SSSUB_TEMP(NAME)	FIXED_OP(sssub,NAME,3)
#define FIXED_USSUB_TEMP(NAME)	FIXED_OP(ussub,NAME,3)
#define FIXED_MUL_TEMP(NAME)	FIXED_OP(mul,NAME,3)
#define FIXED_SSMUL_TEMP(NAME)	FIXED_OP(ssmul,NAME,3)
#define FIXED_USMUL_TEMP(NAME)	FIXED_OP(usmul,NAME,3)
#define FIXED_DIV_TEMP(NAME)	FIXED_OP(div,NAME,3)
#define FIXED_UDIV_TEMP(NAME)	FIXED_OP(udiv,NAME,3)
#define FIXED_SSDIV_TEMP(NAME)	FIXED_OP(ssdiv,NAME,3)
#define FIXED_USDIV_TEMP(NAME)	FIXED_OP(usdiv,NAME,3)
#define FIXED_NEG_TEMP(NAME)	FIXED_OP(neg,NAME,2)
#define FIXED_SSNEG_TEMP(NAME)	FIXED_OP(ssneg,NAME,2)
#define FIXED_USNEG_TEMP(NAME)	FIXED_OP(usneg,NAME,2)
#define FIXED_ASHL_TEMP(NAME)	FIXED_OP(ashl,NAME,3)
#define FIXED_ASHR_TEMP(NAME)	FIXED_OP(ashr,NAME,3)
#define FIXED_LSHR_TEMP(NAME)	FIXED_OP(lshr,NAME,3)
#define FIXED_SSASHL_TEMP(NAME)	FIXED_OP(ssashl,NAME,3)
#define FIXED_USASHL_TEMP(NAME)	FIXED_OP(usashl,NAME,3)
#define FIXED_CMP_TEMP(NAME)	FIXED_OP(cmp,NAME,2)

#if defined (MODE_NAME)
#if defined (DINT_C_TYPE)
#define FIXED_SATURATE1	FIXED_SATURATE1_TEMP(MODE_NAME_S)
#else
#define FIXED_SATURATE2	FIXED_SATURATE2_TEMP(MODE_NAME_S)
#endif
#define FIXED_MULHELPER	FIXED_MULHELPER_TEMP(MODE_NAME_S)
#define FIXED_DIVHELPER	FIXED_DIVHELPER_TEMP(MODE_NAME_S)
#define FIXED_ASHLHELPER	FIXED_ASHLHELPER_TEMP(MODE_NAME_S)
#define FIXED_ADD	FIXED_ADD_TEMP(MODE_NAME_S)
#define FIXED_SUB	FIXED_SUB_TEMP(MODE_NAME_S)
#define FIXED_MUL	FIXED_MUL_TEMP(MODE_NAME_S)
#define FIXED_NEG	FIXED_NEG_TEMP(MODE_NAME_S)
#define FIXED_ASHL	FIXED_ASHL_TEMP(MODE_NAME_S)
#define FIXED_CMP	FIXED_CMP_TEMP(MODE_NAME_S)

/* The following functions are for all fixed-point modes.  */
#if defined (DINT_C_TYPE)
extern void FIXED_SATURATE1 (DINT_C_TYPE *);
#else
extern void FIXED_SATURATE2 (INT_C_TYPE *, INT_C_TYPE *);
#endif
extern FIXED_C_TYPE FIXED_MULHELPER (FIXED_C_TYPE, FIXED_C_TYPE, word_type);
extern FIXED_C_TYPE FIXED_DIVHELPER (FIXED_C_TYPE, FIXED_C_TYPE, word_type);
extern FIXED_C_TYPE FIXED_ASHLHELPER (FIXED_C_TYPE, word_type, word_type);
extern FIXED_C_TYPE FIXED_ADD (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_SUB (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_MUL (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_NEG (FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_ASHL (FIXED_C_TYPE, word_type);
extern word_type FIXED_CMP (FIXED_C_TYPE, FIXED_C_TYPE);
#endif

#if MODE_UNSIGNED == 0 /* Signed types.  */
#define PADDING_BITS	(FIXED_WIDTH - 1 - I_F_BITS)
#define NONPADDING_BITS	(1 + I_F_BITS)

#if defined (MODE_NAME)
#define FIXED_DIV	FIXED_DIV_TEMP(MODE_NAME_S)
#define FIXED_ASHR	FIXED_ASHR_TEMP(MODE_NAME_S)
#define FIXED_SSADD	FIXED_SSADD_TEMP(MODE_NAME_S)
#define FIXED_SSSUB	FIXED_SSSUB_TEMP(MODE_NAME_S)
#define FIXED_SSMUL	FIXED_SSMUL_TEMP(MODE_NAME_S)
#define FIXED_SSDIV	FIXED_SSDIV_TEMP(MODE_NAME_S)
#define FIXED_SSNEG	FIXED_SSNEG_TEMP(MODE_NAME_S)
#define FIXED_SSASHL	FIXED_SSASHL_TEMP(MODE_NAME_S)

/* The following functions are for signed fixed-point modes.  */
extern FIXED_C_TYPE FIXED_DIV (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_ASHR (FIXED_C_TYPE, word_type);
extern FIXED_C_TYPE FIXED_SSADD (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_SSSUB (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_SSMUL (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_SSDIV (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_SSNEG (FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_SSASHL (FIXED_C_TYPE, word_type);
#endif

#else /* Unsigned types.  */
#define PADDING_BITS	(FIXED_WIDTH - I_F_BITS)
#define NONPADDING_BITS	(I_F_BITS)

#if defined (MODE_NAME)
#define FIXED_UDIV	FIXED_UDIV_TEMP(MODE_NAME_S)
#define FIXED_LSHR	FIXED_LSHR_TEMP(MODE_NAME_S)
#define FIXED_USDIV	FIXED_USDIV_TEMP(MODE_NAME_S)
#define FIXED_USADD	FIXED_USADD_TEMP(MODE_NAME_S)
#define FIXED_USSUB	FIXED_USSUB_TEMP(MODE_NAME_S)
#define FIXED_USMUL	FIXED_USMUL_TEMP(MODE_NAME_S)
#define FIXED_USNEG	FIXED_USNEG_TEMP(MODE_NAME_S)
#define FIXED_USASHL	FIXED_USASHL_TEMP(MODE_NAME_S)

/* The following functions are for unsigned fixed-point modes.  */
extern FIXED_C_TYPE FIXED_UDIV (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_LSHR (FIXED_C_TYPE, word_type);
extern FIXED_C_TYPE FIXED_USADD (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_USSUB (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_USMUL (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_USDIV (FIXED_C_TYPE, FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_USNEG (FIXED_C_TYPE);
extern FIXED_C_TYPE FIXED_USASHL (FIXED_C_TYPE, word_type);
#endif

#endif /* End of testing MODE_UNSIGNED.  */

/* This define is to check if this mode have any padding bits.  */
#define HAVE_PADDING_BITS	(PADDING_BITS > 0)

/* ------------------------------------------------------------------------ */
/* The following defines are for conversions.  */

#if defined (FROM_QI) && HAVE_QI == 1
#define FROM_TYPE		1	/* Signed integer.  */
#define FROM_INT_C_TYPE		QItype
#define FROM_SINT_C_TYPE	QItype
#define FROM_UINT_C_TYPE	UQItype
#define FROM_MODE_NAME_S	qi
#define FROM_INT_SIZE		1	/* in bytes.  */

#elif defined (FROM_HI) && HAVE_HI == 1
#define FROM_TYPE		1	/* Signed integer.  */
#define FROM_INT_C_TYPE		HItype
#define FROM_SINT_C_TYPE	HItype
#define FROM_UINT_C_TYPE	UHItype
#define FROM_MODE_NAME_S	hi
#define FROM_INT_SIZE		2	/* in bytes.  */

#elif defined (FROM_SI) && HAVE_SI == 1
#define FROM_TYPE		1	/* Signed integer.  */
#define FROM_INT_C_TYPE		SItype
#define FROM_SINT_C_TYPE	SItype
#define FROM_UINT_C_TYPE	USItype
#define FROM_MODE_NAME_S	si
#define FROM_INT_SIZE		4	/* in bytes.  */

#elif defined (FROM_DI) && HAVE_DI == 1
#define FROM_TYPE		1	/* Signed integer.  */
#define FROM_INT_C_TYPE		DItype
#define FROM_SINT_C_TYPE	DItype
#define FROM_UINT_C_TYPE	UDItype
#define FROM_MODE_NAME_S	di
#define FROM_INT_SIZE		8	/* in bytes.  */

#elif defined (FROM_TI) && HAVE_TI == 1
#define FROM_TYPE		1	/* Signed integer.  */
#define FROM_INT_C_TYPE		TItype
#define FROM_SINT_C_TYPE	TItype
#define FROM_UINT_C_TYPE	UTItype
#define FROM_MODE_NAME_S	ti
#define FROM_INT_SIZE		16	/* in bytes.  */

#elif defined (FROM_UQI) && HAVE_UQI == 1
#define FROM_TYPE		2	/* Unsigned integer.  */
#define FROM_INT_C_TYPE		QItype
#define FROM_SINT_C_TYPE	QItype
#define FROM_UINT_C_TYPE	UQItype
#define FROM_MODE_NAME_S	qi
#define FROM_INT_SIZE		1	/* in bytes.  */

#elif defined (FROM_UHI) && HAVE_UHI == 1
#define FROM_TYPE		2	/* Unsigned integer.  */
#define FROM_INT_C_TYPE		UHItype
#define FROM_SINT_C_TYPE	HItype
#define FROM_UINT_C_TYPE	UHItype
#define FROM_MODE_NAME_S	hi
#define FROM_INT_SIZE		2	/* in bytes.  */

#elif defined (FROM_USI) && HAVE_USI == 1
#define FROM_TYPE		2	/* Unsigned integer.  */
#define FROM_INT_C_TYPE		USItype
#define FROM_SINT_C_TYPE	SItype
#define FROM_UINT_C_TYPE	USItype
#define FROM_MODE_NAME_S	si
#define FROM_INT_SIZE		4	/* in bytes.  */

#elif defined (FROM_UDI) && HAVE_UDI == 1
#define FROM_TYPE		2	/* Unsigned integer.  */
#define FROM_INT_C_TYPE		UDItype
#define FROM_SINT_C_TYPE	DItype
#define FROM_UINT_C_TYPE	UDItype
#define FROM_MODE_NAME_S	di
#define FROM_INT_SIZE		8	/* in bytes.  */

#elif defined (FROM_UTI) && HAVE_UTI == 1
#define FROM_TYPE		2	/* Unsigned integer.  */
#define FROM_INT_C_TYPE		UTItype
#define FROM_SINT_C_TYPE	TItype
#define FROM_UINT_C_TYPE	UTItype
#define FROM_MODE_NAME_S	ti
#define FROM_INT_SIZE		16	/* in bytes.  */

#elif defined (FROM_SF) && HAVE_SF == 1
#define FROM_TYPE		3	/* Floating-point.  */
#define FROM_FLOAT_C_TYPE	SFtype
#define FROM_MODE_NAME_S	sf

#elif defined (FROM_DF) && HAVE_DF == 1
#define FROM_TYPE		3	/* Floating-point.  */
#define FROM_FLOAT_C_TYPE	DFtype
#define FROM_MODE_NAME_S	df

#elif defined (FROM_QQ) && HAVE_QQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		QQ
#define FROM_MODE_NAME_S	qq
#define FROM_INT_C_TYPE		QItype
#define FROM_SINT_C_TYPE	QItype
#define FROM_UINT_C_TYPE	UQItype
#define FROM_MODE_UNSIGNED	0
#define FROM_FIXED_SIZE		1	/* in bytes.  */

#elif defined (FROM_HQ) && HAVE_HQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		HQ
#define FROM_MODE_NAME_S	hq
#define FROM_INT_C_TYPE		HItype
#define FROM_SINT_C_TYPE	HItype
#define FROM_UINT_C_TYPE	UHItype
#define FROM_MODE_UNSIGNED	0
#define FROM_FIXED_SIZE		2	/* in bytes.  */

#elif defined (FROM_SQ) && HAVE_SQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		SQ
#define FROM_MODE_NAME_S	sq
#define FROM_INT_C_TYPE		SItype
#define FROM_SINT_C_TYPE	SItype
#define FROM_UINT_C_TYPE	USItype
#define FROM_MODE_UNSIGNED	0
#define FROM_FIXED_SIZE		4	/* in bytes.  */

#elif defined (FROM_DQ) && HAVE_DQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		DQ
#define FROM_MODE_NAME_S	dq
#define FROM_INT_C_TYPE		DItype
#define FROM_SINT_C_TYPE	DItype
#define FROM_UINT_C_TYPE	UDItype
#define FROM_MODE_UNSIGNED	0
#define FROM_FIXED_SIZE		8	/* in bytes.  */

#elif defined (FROM_TQ) && HAVE_TQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		TQ
#define FROM_MODE_NAME_S	tq
#define FROM_INT_C_TYPE		TItype
#define FROM_SINT_C_TYPE	TItype
#define FROM_UINT_C_TYPE	UTItype
#define FROM_MODE_UNSIGNED	0
#define FROM_FIXED_SIZE		16	/* in bytes.  */

#elif defined (FROM_UQQ) && HAVE_UQQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		UQQ
#define FROM_MODE_NAME_S	uqq
#define FROM_INT_C_TYPE		UQItype
#define FROM_SINT_C_TYPE	QItype
#define FROM_UINT_C_TYPE	UQItype
#define FROM_MODE_UNSIGNED	1
#define FROM_FIXED_SIZE		1	/* in bytes.  */

#elif defined (FROM_UHQ) && HAVE_UHQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		UHQ
#define FROM_MODE_NAME_S	uhq
#define FROM_INT_C_TYPE		UHItype
#define FROM_SINT_C_TYPE	HItype
#define FROM_UINT_C_TYPE	UHItype
#define FROM_MODE_UNSIGNED	1
#define FROM_FIXED_SIZE		2	/* in bytes.  */

#elif defined (FROM_USQ) && HAVE_USQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		USQ
#define FROM_MODE_NAME_S	usq
#define FROM_INT_C_TYPE		USItype
#define FROM_SINT_C_TYPE	SItype
#define FROM_UINT_C_TYPE	USItype
#define FROM_MODE_UNSIGNED	1
#define FROM_FIXED_SIZE		4	/* in bytes.  */

#elif defined (FROM_UDQ) && HAVE_UDQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		UDQ
#define FROM_MODE_NAME_S	udq
#define FROM_INT_C_TYPE		UDItype
#define FROM_SINT_C_TYPE	DItype
#define FROM_UINT_C_TYPE	UDItype
#define FROM_MODE_UNSIGNED	1
#define FROM_FIXED_SIZE		8	/* in bytes.  */

#elif defined (FROM_UTQ) && HAVE_UTQ == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		UTQ
#define FROM_MODE_NAME_S	utq
#define FROM_INT_C_TYPE		UTItype
#define FROM_SINT_C_TYPE	TItype
#define FROM_UINT_C_TYPE	UTItype
#define FROM_MODE_UNSIGNED	1
#define FROM_FIXED_SIZE		16	/* in bytes.  */

#elif defined (FROM_HA) && HAVE_HA == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		HA
#define FROM_MODE_NAME_S	ha
#define FROM_INT_C_TYPE		HItype
#define FROM_SINT_C_TYPE	HItype
#define FROM_UINT_C_TYPE	UHItype
#define FROM_MODE_UNSIGNED	0
#define FROM_FIXED_SIZE		2	/* in bytes.  */

#elif defined (FROM_SA) && HAVE_SA == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		SA
#define FROM_MODE_NAME_S	sa
#define FROM_INT_C_TYPE		SItype
#define FROM_SINT_C_TYPE	SItype
#define FROM_UINT_C_TYPE	USItype
#define FROM_MODE_UNSIGNED	0
#define FROM_FIXED_SIZE		4	/* in bytes.  */

#elif defined (FROM_DA) && HAVE_DA == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		DA
#define FROM_MODE_NAME_S	da
#define FROM_INT_C_TYPE		DItype
#define FROM_SINT_C_TYPE	DItype
#define FROM_UINT_C_TYPE	UDItype
#define FROM_MODE_UNSIGNED	0
#define FROM_FIXED_SIZE		8	/* in bytes.  */

#elif defined (FROM_TA) && HAVE_TA == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		TA
#define FROM_MODE_NAME_S	ta
#define FROM_INT_C_TYPE		TItype
#define FROM_SINT_C_TYPE	TItype
#define FROM_UINT_C_TYPE	UTItype
#define FROM_MODE_UNSIGNED	0
#define FROM_FIXED_SIZE		16	/* in bytes.  */

#elif defined (FROM_UHA) && HAVE_UHA == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		UHA
#define FROM_MODE_NAME_S	uha
#define FROM_INT_C_TYPE		UHItype
#define FROM_SINT_C_TYPE	HItype
#define FROM_UINT_C_TYPE	UHItype
#define FROM_MODE_UNSIGNED	1
#define FROM_FIXED_SIZE		2	/* in bytes.  */

#elif defined (FROM_USA) && HAVE_USA == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		USA
#define FROM_MODE_NAME_S	usa
#define FROM_INT_C_TYPE		USItype
#define FROM_SINT_C_TYPE	SItype
#define FROM_UINT_C_TYPE	USItype
#define FROM_MODE_UNSIGNED	1
#define FROM_FIXED_SIZE		4	/* in bytes.  */

#elif defined (FROM_UDA) && HAVE_UDA == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		UDA
#define FROM_MODE_NAME_S	uda
#define FROM_INT_C_TYPE		UDItype
#define FROM_SINT_C_TYPE	DItype
#define FROM_UINT_C_TYPE	UDItype
#define FROM_MODE_UNSIGNED	1
#define FROM_FIXED_SIZE		8	/* in bytes.  */

#elif defined (FROM_UTA) && HAVE_UTA == 1
#define FROM_TYPE		4	/* Fixed-point.  */
#define FROM_MODE_NAME		UTA
#define FROM_MODE_NAME_S	uta
#define FROM_INT_C_TYPE		UTItype
#define FROM_SINT_C_TYPE	TItype
#define FROM_UINT_C_TYPE	UTItype
#define FROM_MODE_UNSIGNED	1
#define FROM_FIXED_SIZE		16	/* in bytes.  */

#endif

#if defined (TO_QI) && HAVE_QI == 1 && !defined (FROM_QI)
#define TO_TYPE			1	/* Signed integer.  */
#define TO_INT_C_TYPE		QItype
#define TO_SINT_C_TYPE		QItype
#define TO_UINT_C_TYPE		UQItype
#define TO_MODE_NAME_S		qi

#elif defined (TO_HI) && HAVE_HI == 1 && !defined (FROM_HI)
#define TO_TYPE			1	/* Signed integer.  */
#define TO_INT_C_TYPE		HItype
#define TO_SINT_C_TYPE		HItype
#define TO_UINT_C_TYPE		UHItype
#define TO_MODE_NAME_S		hi

#elif defined (TO_SI) && HAVE_SI == 1 && !defined (FROM_SI)
#define TO_TYPE			1	/* Signed integer.  */
#define TO_INT_C_TYPE		SItype
#define TO_SINT_C_TYPE		SItype
#define TO_UINT_C_TYPE		USItype
#define TO_MODE_NAME_S		si

#elif defined (TO_DI) && HAVE_DI == 1 && !defined (FROM_DI)
#define TO_TYPE			1	/* Signed integer.  */
#define TO_INT_C_TYPE		DItype
#define TO_SINT_C_TYPE		DItype
#define TO_UINT_C_TYPE		UDItype
#define TO_MODE_NAME_S		di

#elif defined (TO_TI) && HAVE_TI == 1 && !defined (FROM_TI)
#define TO_TYPE			1	/* Signed integer.  */
#define TO_INT_C_TYPE		TItype
#define TO_SINT_C_TYPE		TItype
#define TO_UINT_C_TYPE		UTItype
#define TO_MODE_NAME_S		ti

#elif defined (TO_UQI) && HAVE_UQI == 1 && !defined (FROM_UQI)
#define TO_TYPE			2	/* Unsigned integer.  */
#define TO_INT_C_TYPE		UQItype
#define TO_SINT_C_TYPE		QItype
#define TO_UINT_C_TYPE		UQItype
#define TO_MODE_NAME_S		qi

#elif defined (TO_UHI) && HAVE_UHI == 1 && !defined (FROM_UHI)
#define TO_TYPE			2	/* Unsigned integer.  */
#define TO_INT_C_TYPE		UHItype
#define TO_SINT_C_TYPE		HItype
#define TO_UINT_C_TYPE		UHItype
#define TO_MODE_NAME_S		hi

#elif defined (TO_USI) && HAVE_USI == 1 && !defined (FROM_USI)
#define TO_TYPE			2	/* Unsigned integer.  */
#define TO_INT_C_TYPE		USItype
#define TO_SINT_C_TYPE		SItype
#define TO_UINT_C_TYPE		USItype
#define TO_MODE_NAME_S		si

#elif defined (TO_UDI) && HAVE_UDI == 1 && !defined (FROM_UDI)
#define TO_TYPE			2	/* Unsigned integer.  */
#define TO_INT_C_TYPE		UDItype
#define TO_SINT_C_TYPE		DItype
#define TO_UINT_C_TYPE		UDItype
#define TO_MODE_NAME_S		di

#elif defined (TO_UTI) && HAVE_UTI == 1 && !defined (FROM_UTI)
#define TO_TYPE			2	/* Unsigned integer.  */
#define TO_INT_C_TYPE		UTItype
#define TO_SINT_C_TYPE		TItype
#define TO_UINT_C_TYPE		UTItype
#define TO_MODE_NAME_S		ti

#elif defined (TO_SF) && HAVE_SF == 1 && !defined (FROM_SF)
#define TO_TYPE			3	/* Floating-point.  */
#define TO_FLOAT_C_TYPE		SFtype
#define TO_MODE_NAME_S		sf

#elif defined (TO_DF) && HAVE_DF == 1 && !defined (FROM_DF)
#define TO_TYPE			3	/* Floating-point.  */
#define TO_FLOAT_C_TYPE		DFtype
#define TO_MODE_NAME_S		df

#elif defined (TO_QQ) && HAVE_QQ == 1 && !defined (FROM_QQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		QQ
#define TO_MODE_NAME_S		qq
#define TO_INT_C_TYPE		QItype
#define TO_SINT_C_TYPE		QItype
#define TO_UINT_C_TYPE		UQItype
#define TO_MODE_UNSIGNED	0
#define TO_FIXED_SIZE		1	/* in bytes.  */

#elif defined (TO_HQ) && HAVE_HQ == 1 && !defined (FROM_HQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		HQ
#define TO_MODE_NAME_S		hq
#define TO_INT_C_TYPE		HItype
#define TO_SINT_C_TYPE		HItype
#define TO_UINT_C_TYPE		UHItype
#define TO_MODE_UNSIGNED	0
#define TO_FIXED_SIZE		2	/* in bytes.  */

#elif defined (TO_SQ) && HAVE_SQ == 1 && !defined (FROM_SQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		SQ
#define TO_MODE_NAME_S		sq
#define TO_INT_C_TYPE		SItype
#define TO_SINT_C_TYPE		SItype
#define TO_UINT_C_TYPE		USItype
#define TO_MODE_UNSIGNED	0
#define TO_FIXED_SIZE		4	/* in bytes.  */

#elif defined (TO_DQ) && HAVE_DQ == 1 && !defined (FROM_DQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		DQ
#define TO_MODE_NAME_S		dq
#define TO_INT_C_TYPE		DItype
#define TO_SINT_C_TYPE		DItype
#define TO_UINT_C_TYPE		UDItype
#define TO_MODE_UNSIGNED	0
#define TO_FIXED_SIZE		8	/* in bytes.  */

#elif defined (TO_TQ) && HAVE_TQ == 1 && !defined (FROM_TQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		TQ
#define TO_MODE_NAME_S		tq
#define TO_INT_C_TYPE		TItype
#define TO_SINT_C_TYPE		TItype
#define TO_UINT_C_TYPE		UTItype
#define TO_MODE_UNSIGNED	0
#define TO_FIXED_SIZE		16	/* in bytes.  */

#elif defined (TO_UQQ) && HAVE_UQQ == 1 && !defined (FROM_UQQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		UQQ
#define TO_MODE_NAME_S		uqq
#define TO_INT_C_TYPE		UQItype
#define TO_SINT_C_TYPE		QItype
#define TO_UINT_C_TYPE		UQItype
#define TO_MODE_UNSIGNED	1
#define TO_FIXED_SIZE		1	/* in bytes.  */

#elif defined (TO_UHQ) && HAVE_UHQ == 1 && !defined (FROM_UHQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		UHQ
#define TO_MODE_NAME_S		uhq
#define TO_INT_C_TYPE		UHItype
#define TO_SINT_C_TYPE		HItype
#define TO_UINT_C_TYPE		UHItype
#define TO_MODE_UNSIGNED	1
#define TO_FIXED_SIZE		2	/* in bytes.  */

#elif defined (TO_USQ) && HAVE_USQ == 1 && !defined (FROM_USQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		USQ
#define TO_MODE_NAME_S		usq
#define TO_INT_C_TYPE		USItype
#define TO_SINT_C_TYPE		SItype
#define TO_UINT_C_TYPE		USItype
#define TO_MODE_UNSIGNED	1
#define TO_FIXED_SIZE		4	/* in bytes.  */

#elif defined (TO_UDQ) && HAVE_UDQ == 1 && !defined (FROM_UDQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		UDQ
#define TO_MODE_NAME_S		udq
#define TO_INT_C_TYPE		UDItype
#define TO_SINT_C_TYPE		DItype
#define TO_UINT_C_TYPE		UDItype
#define TO_MODE_UNSIGNED	1
#define TO_FIXED_SIZE		8	/* in bytes.  */

#elif defined (TO_UTQ) && HAVE_UTQ == 1 && !defined (FROM_UTQ)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		UTQ
#define TO_MODE_NAME_S		utq
#define TO_INT_C_TYPE		UTItype
#define TO_SINT_C_TYPE		TItype
#define TO_UINT_C_TYPE		UTItype
#define TO_MODE_UNSIGNED	1
#define TO_FIXED_SIZE		16	/* in bytes.  */

#elif defined (TO_HA) && HAVE_HA == 1 && !defined (FROM_HA)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		HA
#define TO_MODE_NAME_S		ha
#define TO_INT_C_TYPE		HItype
#define TO_SINT_C_TYPE		HItype
#define TO_UINT_C_TYPE		UHItype
#define TO_MODE_UNSIGNED	0
#define TO_FIXED_SIZE		2	/* in bytes.  */

#elif defined (TO_SA) && HAVE_SA == 1 && !defined (FROM_SA)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		SA
#define TO_MODE_NAME_S		sa
#define TO_INT_C_TYPE		SItype
#define TO_SINT_C_TYPE		SItype
#define TO_UINT_C_TYPE		USItype
#define TO_MODE_UNSIGNED	0
#define TO_FIXED_SIZE		4	/* in bytes.  */

#elif defined (TO_DA) && HAVE_DA == 1 && !defined (FROM_DA)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		DA
#define TO_MODE_NAME_S		da
#define TO_INT_C_TYPE		DItype
#define TO_SINT_C_TYPE		DItype
#define TO_UINT_C_TYPE		UDItype
#define TO_MODE_UNSIGNED	0
#define TO_FIXED_SIZE		8	/* in bytes.  */

#elif defined (TO_TA) && HAVE_TA == 1 && !defined (FROM_TA)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		TA
#define TO_MODE_NAME_S		ta
#define TO_INT_C_TYPE		TItype
#define TO_SINT_C_TYPE		TItype
#define TO_UINT_C_TYPE		UTItype
#define TO_MODE_UNSIGNED	0
#define TO_FIXED_SIZE		16	/* in bytes.  */

#elif defined (TO_UHA) && HAVE_UHA == 1 && !defined (FROM_UHA)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		UHA
#define TO_MODE_NAME_S		uha
#define TO_INT_C_TYPE		UHItype
#define TO_SINT_C_TYPE		HItype
#define TO_UINT_C_TYPE		UHItype
#define TO_MODE_UNSIGNED	1
#define TO_FIXED_SIZE		2	/* in bytes.  */

#elif defined (TO_USA) && HAVE_USA == 1 && !defined (FROM_USA)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		USA
#define TO_MODE_NAME_S		usa
#define TO_INT_C_TYPE		USItype
#define TO_SINT_C_TYPE		SItype
#define TO_UINT_C_TYPE		USItype
#define TO_MODE_UNSIGNED	1
#define TO_FIXED_SIZE		4	/* in bytes.  */

#elif defined (TO_UDA) && HAVE_UDA == 1 && !defined (FROM_UDA)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		UDA
#define TO_MODE_NAME_S		uda
#define TO_INT_C_TYPE		UDItype
#define TO_SINT_C_TYPE		DItype
#define TO_UINT_C_TYPE		UDItype
#define TO_MODE_UNSIGNED	1
#define TO_FIXED_SIZE		8	/* in bytes.  */

#elif defined (TO_UTA) && HAVE_UTA == 1 && !defined (FROM_UTA)
#define TO_TYPE			4	/* Fixed-point.  */
#define TO_MODE_NAME		UTA
#define TO_MODE_NAME_S		uta
#define TO_INT_C_TYPE		UTItype
#define TO_SINT_C_TYPE		TItype
#define TO_UINT_C_TYPE		UTItype
#define TO_MODE_UNSIGNED	1
#define TO_FIXED_SIZE		16	/* in bytes.  */

#endif

#if defined (FROM_MODE_NAME_S) && defined (TO_MODE_NAME_S)

#if FROM_TYPE == 1	/* Signed integer.  */
#define FROM_INT_WIDTH		(FROM_INT_SIZE * __CHAR_BIT__)
#endif

#if FROM_TYPE == 2	/* Unsigned integer.  */
#define FROM_INT_WIDTH		(FROM_INT_SIZE * __CHAR_BIT__)
#endif

#if FROM_TYPE == 4	/* Fixed-point.  */
#define FROM_FIXED_C_TYPE	FIXED_C_TYPE2(FROM_MODE_NAME)
#define FROM_FBITS		FBITS2(FROM_MODE_NAME)
#define FROM_FIXED_WIDTH	(FROM_FIXED_SIZE * __CHAR_BIT__)
#define FROM_FBITS		FBITS2(FROM_MODE_NAME)
#define FROM_IBITS		IBITS2(FROM_MODE_NAME)
#define FROM_I_F_BITS		(FROM_FBITS + FROM_IBITS)

#if FROM_MODE_UNSIGNED == 0 /* Signed types.  */
#define FROM_PADDING_BITS	(FROM_FIXED_WIDTH - 1 - FROM_I_F_BITS)
#define FROM_NONPADDING_BITS	(1 + FROM_I_F_BITS)
#else /* Unsigned types.  */
#define FROM_PADDING_BITS	(FROM_FIXED_WIDTH - FROM_I_F_BITS)
#define FROM_NONPADDING_BITS	(FROM_I_F_BITS)
#endif
#define FROM_HAVE_PADDING_BITS	(FROM_PADDING_BITS > 0)
#endif /* FROM_TYPE == 4  */

#if TO_TYPE == 4	/* Fixed-point.  */
#define TO_FIXED_C_TYPE		FIXED_C_TYPE2(TO_MODE_NAME)
#define TO_FBITS		FBITS2(TO_MODE_NAME)
#define TO_FIXED_WIDTH		(TO_FIXED_SIZE * __CHAR_BIT__)
#define TO_FBITS		FBITS2(TO_MODE_NAME)
#define TO_IBITS		IBITS2(TO_MODE_NAME)
#define TO_I_F_BITS		(TO_FBITS + TO_IBITS)

#if TO_MODE_UNSIGNED == 0 /* Signed types.  */
#define TO_PADDING_BITS		(TO_FIXED_WIDTH - 1 - TO_I_F_BITS)
#define TO_NONPADDING_BITS	(1 + TO_I_F_BITS)
#else /* Unsigned types.  */
#define TO_PADDING_BITS		(TO_FIXED_WIDTH - TO_I_F_BITS)
#define TO_NONPADDING_BITS	(TO_I_F_BITS)
#endif
#define TO_HAVE_PADDING_BITS	(TO_PADDING_BITS > 0)
#endif /* TO_TYPE == 4  */

#ifdef LIBGCC2_FIXEDBIT_GNU_PREFIX
#define FIXED_CONVERT_OP(OP,FROM,TO)	__gnu_ ## OP ## FROM ## TO
#define FIXED_CONVERT_OP2(OP,FROM,TO)	__gnu_ ## OP ## FROM ## TO ## 2
#else
#define FIXED_CONVERT_OP(OP,FROM,TO)	__ ## OP ## FROM ## TO
#define FIXED_CONVERT_OP2(OP,FROM,TO)	__ ## OP ## FROM ## TO ## 2
#endif
#define FRACT_TEMP(N1,N2)		FIXED_CONVERT_OP(fract,N1,N2)
#define FRACT2_TEMP(N1,N2)		FIXED_CONVERT_OP2(fract,N1,N2)
#define SATFRACT_TEMP(N1,N2)		FIXED_CONVERT_OP(satfract,N1,N2)
#define SATFRACT2_TEMP(N1,N2)		FIXED_CONVERT_OP2(satfract,N1,N2)
#define FRACTUNS_TEMP(N1,N2)		FIXED_CONVERT_OP(fractuns,N1,N2)
#define SATFRACTUNS_TEMP(N1,N2)		FIXED_CONVERT_OP(satfractuns,N1,N2)

/* Define conversions from fixed-point to fixed-point.  */
#if FROM_TYPE == 4 && TO_TYPE == 4

#if FROM_FIXED_SIZE > TO_FIXED_SIZE
#define BIG_SINT_C_TYPE	FROM_SINT_C_TYPE
#define BIG_UINT_C_TYPE	FROM_UINT_C_TYPE
#define BIG_WIDTH	FROM_FIXED_WIDTH
#else
#define BIG_SINT_C_TYPE	TO_SINT_C_TYPE
#define BIG_UINT_C_TYPE	TO_UINT_C_TYPE
#define BIG_WIDTH	TO_FIXED_WIDTH
#endif

/* Check if FROM* and TO* are in the same machine class.  */
#if ((FROM_MODE_UNSIGNED == TO_MODE_UNSIGNED) \
     && ((FROM_IBITS == 0) == (TO_IBITS == 0)))
/* Same modes: append '2' to conversion function names */
#define FRACT		FRACT2_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
#define SATFRACT	SATFRACT2_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
#else
/* Different modes: don't append '2' to conversion function names */
#define FRACT		FRACT_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
#define SATFRACT	SATFRACT_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
#endif

extern TO_FIXED_C_TYPE FRACT (FROM_FIXED_C_TYPE);
extern TO_FIXED_C_TYPE SATFRACT (FROM_FIXED_C_TYPE);
#endif /* FROM_TYPE == 4 && TO_TYPE == 4  */

/* Define conversions from fixed-point to signed integer.  */
#if FROM_TYPE == 4 && TO_TYPE == 1
#define FRACT		FRACT_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
extern TO_INT_C_TYPE	FRACT (FROM_FIXED_C_TYPE);
#endif /* FROM_TYPE == 4 && TO_TYPE == 1  */

/* Define conversions from fixed-point to unsigned integer.  */
#if FROM_TYPE == 4 && TO_TYPE == 2
#define FRACTUNS	FRACTUNS_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
extern TO_INT_C_TYPE 	FRACTUNS (FROM_FIXED_C_TYPE);
#endif /* FROM_TYPE == 4 && TO_TYPE == 2  */

/* Define conversions from fixed-point to floating-point.  */
#if FROM_TYPE == 4 && TO_TYPE == 3
#define BASE1(NUM)	0x1.0p ## NUM
#define BASE2(NUM)	BASE1(NUM)
#define BASE		BASE2(FROM_FBITS)
#define FRACT		FRACT_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
extern TO_FLOAT_C_TYPE	FRACT (FROM_FIXED_C_TYPE);
#endif /* FROM_TYPE == 4 && TO_TYPE == 3  */

/* Define conversions from signed integer to fixed-point.  */
#if FROM_TYPE == 1 && TO_TYPE == 4

#if FROM_INT_SIZE > TO_FIXED_SIZE
#define BIG_SINT_C_TYPE	FROM_SINT_C_TYPE
#define BIG_UINT_C_TYPE	FROM_UINT_C_TYPE
#define BIG_WIDTH	FROM_INT_WIDTH
#else
#define BIG_SINT_C_TYPE	TO_SINT_C_TYPE
#define BIG_UINT_C_TYPE	TO_UINT_C_TYPE
#define BIG_WIDTH	TO_FIXED_WIDTH
#endif

#define FRACT		FRACT_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
#define SATFRACT	SATFRACT_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
extern TO_FIXED_C_TYPE	FRACT (FROM_INT_C_TYPE);
extern TO_FIXED_C_TYPE	SATFRACT (FROM_INT_C_TYPE);
#endif /* FROM_TYPE == 1 && TO_TYPE == 4  */

/* Define conversions from unsigned integer to fixed-point.  */
#if FROM_TYPE == 2 && TO_TYPE == 4

#if FROM_INT_SIZE > TO_FIXED_SIZE
#define BIG_SINT_C_TYPE	FROM_SINT_C_TYPE
#define BIG_UINT_C_TYPE	FROM_UINT_C_TYPE
#define BIG_WIDTH	FROM_INT_WIDTH
#else
#define BIG_SINT_C_TYPE	TO_SINT_C_TYPE
#define BIG_UINT_C_TYPE	TO_UINT_C_TYPE
#define BIG_WIDTH	TO_FIXED_WIDTH
#endif

#define FRACTUNS	FRACTUNS_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
#define SATFRACTUNS	SATFRACTUNS_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
extern TO_FIXED_C_TYPE	FRACTUNS (FROM_INT_C_TYPE);
extern TO_FIXED_C_TYPE	SATFRACTUNS (FROM_INT_C_TYPE);
#endif /* FROM_TYPE == 2 && TO_TYPE == 4  */

/* Define conversions from floating-point to fixed-point.  */
#if FROM_TYPE == 3 && TO_TYPE == 4

#define BASE1(NUM)	(0x1.0p ## NUM)
#define BASE2(NUM)	BASE1(NUM)
#define BASE		BASE2(TO_FBITS)

#define FIXED_MAX1(NUM1,NUM2)	(0x1.0p ## NUM1 - 0x1.0p- ## NUM2)
#define FIXED_MAX2(NUM1,NUM2)	FIXED_MAX1(NUM1,NUM2)
#define FIXED_MAX	FIXED_MAX2(TO_IBITS,TO_FBITS)

#define FIXED_MIN1(NUM)	(-0x1.0p ## NUM)
#define FIXED_MIN2(NUM)	FIXED_MIN1(NUM)
#if TO_MODE_UNSIGNED == 0
#define FIXED_MIN	FIXED_MIN2(TO_IBITS)
#else
#define FIXED_MIN	0.0
#endif

#define FRACT		FRACT_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
#define SATFRACT	SATFRACT_TEMP(FROM_MODE_NAME_S,TO_MODE_NAME_S)
extern TO_FIXED_C_TYPE	FRACT (FROM_FLOAT_C_TYPE);
extern TO_FIXED_C_TYPE	SATFRACT (FROM_FLOAT_C_TYPE);
#endif /* FROM_TYPE == 3 && TO_TYPE == 4  */

#endif /* defined (FROM_MODE_NAME_S) && defined (TO_MODE_NAME_S)  */

#endif  /* _FIXED_BIT_H */

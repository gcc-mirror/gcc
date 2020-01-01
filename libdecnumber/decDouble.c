/* decDouble module for the decNumber C Library.
   Copyright (C) 2007-2020 Free Software Foundation, Inc.
   Contributed by IBM Corporation.  Author Mike Cowlishaw.

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

/* ------------------------------------------------------------------ */
/* decDouble.c -- decDouble operations module			      */
/* ------------------------------------------------------------------ */
/* This module comprises decDouble operations (including conversions) */
/* ------------------------------------------------------------------ */

#include "decContext.h"       /* public includes */
#include "decDouble.h"	      /* .. */

/* Constant mappings for shared code */
#define DECPMAX     DECDOUBLE_Pmax
#define DECEMIN     DECDOUBLE_Emin
#define DECEMAX     DECDOUBLE_Emax
#define DECEMAXD    DECDOUBLE_EmaxD
#define DECBYTES    DECDOUBLE_Bytes
#define DECSTRING   DECDOUBLE_String
#define DECECONL    DECDOUBLE_EconL
#define DECBIAS     DECDOUBLE_Bias
#define DECLETS     DECDOUBLE_Declets
#define DECQTINY    (-DECDOUBLE_Bias)
/* parameters of next-wider format */
#define DECWBYTES   DECQUAD_Bytes
#define DECWPMAX    DECQUAD_Pmax
#define DECWECONL   DECQUAD_EconL
#define DECWBIAS    DECQUAD_Bias

/* Type and function mappings for shared code */
#define decFloat		   decDouble	  /* Type name */
#define decFloatWider		   decQuad	  /* Type name */

/* Utilities and conversions (binary results, extractors, etc.) */
#define decFloatFromBCD 	   decDoubleFromBCD
#define decFloatFromInt32	   decDoubleFromInt32
#define decFloatFromPacked	   decDoubleFromPacked
#define decFloatFromPackedChecked  decDoubleFromPackedChecked
#define decFloatFromString	   decDoubleFromString
#define decFloatFromUInt32	   decDoubleFromUInt32
#define decFloatFromWider	   decDoubleFromWider
#define decFloatGetCoefficient	   decDoubleGetCoefficient
#define decFloatGetExponent	   decDoubleGetExponent
#define decFloatSetCoefficient	   decDoubleSetCoefficient
#define decFloatSetExponent	   decDoubleSetExponent
#define decFloatShow		   decDoubleShow
#define decFloatToBCD		   decDoubleToBCD
#define decFloatToEngString	   decDoubleToEngString
#define decFloatToInt32 	   decDoubleToInt32
#define decFloatToInt32Exact	   decDoubleToInt32Exact
#define decFloatToPacked	   decDoubleToPacked
#define decFloatToString	   decDoubleToString
#define decFloatToUInt32	   decDoubleToUInt32
#define decFloatToUInt32Exact	   decDoubleToUInt32Exact
#define decFloatToWider 	   decDoubleToWider
#define decFloatZero		   decDoubleZero

/* Computational (result is a decFloat) */
#define decFloatAbs		   decDoubleAbs
#define decFloatAdd		   decDoubleAdd
#define decFloatAnd		   decDoubleAnd
#define decFloatDivide		   decDoubleDivide
#define decFloatDivideInteger	   decDoubleDivideInteger
#define decFloatFMA		   decDoubleFMA
#define decFloatInvert		   decDoubleInvert
#define decFloatLogB		   decDoubleLogB
#define decFloatMax		   decDoubleMax
#define decFloatMaxMag		   decDoubleMaxMag
#define decFloatMin		   decDoubleMin
#define decFloatMinMag		   decDoubleMinMag
#define decFloatMinus		   decDoubleMinus
#define decFloatMultiply	   decDoubleMultiply
#define decFloatNextMinus	   decDoubleNextMinus
#define decFloatNextPlus	   decDoubleNextPlus
#define decFloatNextToward	   decDoubleNextToward
#define decFloatOr		   decDoubleOr
#define decFloatPlus		   decDoublePlus
#define decFloatQuantize	   decDoubleQuantize
#define decFloatReduce		   decDoubleReduce
#define decFloatRemainder	   decDoubleRemainder
#define decFloatRemainderNear	   decDoubleRemainderNear
#define decFloatRotate		   decDoubleRotate
#define decFloatScaleB		   decDoubleScaleB
#define decFloatShift		   decDoubleShift
#define decFloatSubtract	   decDoubleSubtract
#define decFloatToIntegralValue    decDoubleToIntegralValue
#define decFloatToIntegralExact    decDoubleToIntegralExact
#define decFloatXor		   decDoubleXor

/* Comparisons */
#define decFloatCompare 	   decDoubleCompare
#define decFloatCompareSignal	   decDoubleCompareSignal
#define decFloatCompareTotal	   decDoubleCompareTotal
#define decFloatCompareTotalMag    decDoubleCompareTotalMag

/* Copies */
#define decFloatCanonical	   decDoubleCanonical
#define decFloatCopy		   decDoubleCopy
#define decFloatCopyAbs 	   decDoubleCopyAbs
#define decFloatCopyNegate	   decDoubleCopyNegate
#define decFloatCopySign	   decDoubleCopySign

/* Non-computational */
#define decFloatClass		   decDoubleClass
#define decFloatClassString	   decDoubleClassString
#define decFloatDigits		   decDoubleDigits
#define decFloatIsCanonical	   decDoubleIsCanonical
#define decFloatIsFinite	   decDoubleIsFinite
#define decFloatIsInfinite	   decDoubleIsInfinite
#define decFloatIsInteger	   decDoubleIsInteger
#define decFloatIsNaN		   decDoubleIsNaN
#define decFloatIsNormal	   decDoubleIsNormal
#define decFloatIsSignaling	   decDoubleIsSignaling
#define decFloatIsSignalling	   decDoubleIsSignalling
#define decFloatIsSigned	   decDoubleIsSigned
#define decFloatIsSubnormal	   decDoubleIsSubnormal
#define decFloatIsZero		   decDoubleIsZero
#define decFloatRadix		   decDoubleRadix
#define decFloatSameQuantum	   decDoubleSameQuantum
#define decFloatVersion 	   decDoubleVersion

#include "decNumberLocal.h"   /* local includes (need DECPMAX) */
#include "decCommon.c"	      /* non-arithmetic decFloat routines */
#include "decBasic.c"	      /* basic formats routines */


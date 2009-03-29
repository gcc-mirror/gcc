/* decQuad module for the decNumber C Library.
   Copyright (C) 2007 Free Software Foundation, Inc.
   Contributed by IBM Corporation.  Author Mike Cowlishaw.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2, or (at your option) any later
   version.

   In addition to the permissions in the GNU General Public License,
   the Free Software Foundation gives you unlimited permission to link
   the compiled version of this file into combinations with other
   programs, and to distribute those combinations without any
   restriction coming from the use of this file.  (The General Public
   License restrictions do apply in other respects; for example, they
   cover modification of the file, and distribution when not linked
   into a combine executable.)

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* ------------------------------------------------------------------ */
/* decQuad.c -- decQuad operations module			      */
/* ------------------------------------------------------------------ */
/* This module comprises decQuad operations (including conversions)   */
/* ------------------------------------------------------------------ */

#include "decContext.h"	      /* public includes */
#include "decQuad.h"	      /* .. */

/* Constant mappings for shared code */
#define DECPMAX	    DECQUAD_Pmax
#define DECEMIN	    DECQUAD_Emin
#define DECEMAX	    DECQUAD_Emax
#define DECEMAXD    DECQUAD_EmaxD
#define DECBYTES    DECQUAD_Bytes
#define DECSTRING   DECQUAD_String
#define DECECONL    DECQUAD_EconL
#define DECBIAS	    DECQUAD_Bias
#define DECLETS	    DECQUAD_Declets
#define DECQTINY   (-DECQUAD_Bias)

/* Type and function mappings for shared code */
#define decFloat		decQuad		  /* Type name */

/* Utilities and conversions (binary results, extractors, etc.) */
#define decFloatFromBCD		decQuadFromBCD
#define decFloatFromInt32	decQuadFromInt32
#define decFloatFromPacked	decQuadFromPacked
#define decFloatFromString	decQuadFromString
#define decFloatFromUInt32	decQuadFromUInt32
#define decFloatFromWider	decQuadFromWider
#define decFloatGetCoefficient	decQuadGetCoefficient
#define decFloatGetExponent	decQuadGetExponent
#define decFloatSetCoefficient	decQuadSetCoefficient
#define decFloatSetExponent	decQuadSetExponent
#define decFloatShow		decQuadShow
#define decFloatToBCD		decQuadToBCD
#define decFloatToEngString	decQuadToEngString
#define decFloatToInt32		decQuadToInt32
#define decFloatToInt32Exact	decQuadToInt32Exact
#define decFloatToPacked	decQuadToPacked
#define decFloatToString	decQuadToString
#define decFloatToUInt32	decQuadToUInt32
#define decFloatToUInt32Exact	decQuadToUInt32Exact
#define decFloatToWider		decQuadToWider
#define decFloatZero		decQuadZero

/* Computational (result is a decFloat) */
#define decFloatAbs		decQuadAbs
#define decFloatAdd		decQuadAdd
#define decFloatAnd		decQuadAnd
#define decFloatDivide		decQuadDivide
#define decFloatDivideInteger	decQuadDivideInteger
#define decFloatFMA		decQuadFMA
#define decFloatInvert		decQuadInvert
#define decFloatLogB		decQuadLogB
#define decFloatMax		decQuadMax
#define decFloatMaxMag		decQuadMaxMag
#define decFloatMin		decQuadMin
#define decFloatMinMag		decQuadMinMag
#define decFloatMinus		decQuadMinus
#define decFloatMultiply	decQuadMultiply
#define decFloatNextMinus	decQuadNextMinus
#define decFloatNextPlus	decQuadNextPlus
#define decFloatNextToward	decQuadNextToward
#define decFloatOr		decQuadOr
#define decFloatPlus		decQuadPlus
#define decFloatQuantize	decQuadQuantize
#define decFloatReduce		decQuadReduce
#define decFloatRemainder	decQuadRemainder
#define decFloatRemainderNear	decQuadRemainderNear
#define decFloatRotate		decQuadRotate
#define decFloatScaleB		decQuadScaleB
#define decFloatShift		decQuadShift
#define decFloatSubtract	decQuadSubtract
#define decFloatToIntegralValue decQuadToIntegralValue
#define decFloatToIntegralExact decQuadToIntegralExact
#define decFloatXor		decQuadXor

/* Comparisons */
#define decFloatCompare		decQuadCompare
#define decFloatCompareSignal	decQuadCompareSignal
#define decFloatCompareTotal	decQuadCompareTotal
#define decFloatCompareTotalMag decQuadCompareTotalMag

/* Copies */
#define decFloatCanonical	decQuadCanonical
#define decFloatCopy		decQuadCopy
#define decFloatCopyAbs		decQuadCopyAbs
#define decFloatCopyNegate	decQuadCopyNegate
#define decFloatCopySign	decQuadCopySign

/* Non-computational */
#define decFloatClass		decQuadClass
#define decFloatClassString	decQuadClassString
#define decFloatDigits		decQuadDigits
#define decFloatIsCanonical	decQuadIsCanonical
#define decFloatIsFinite	decQuadIsFinite
#define decFloatIsInfinite	decQuadIsInfinite
#define decFloatIsInteger	decQuadIsInteger
#define decFloatIsNaN		decQuadIsNaN
#define decFloatIsNormal	decQuadIsNormal
#define decFloatIsSignaling	decQuadIsSignaling
#define decFloatIsSignalling	decQuadIsSignalling
#define decFloatIsSigned	decQuadIsSigned
#define decFloatIsSubnormal	decQuadIsSubnormal
#define decFloatIsZero		decQuadIsZero
#define decFloatRadix		decQuadRadix
#define decFloatSameQuantum	decQuadSameQuantum
#define decFloatVersion		decQuadVersion


#include "decNumberLocal.h"   /* local includes (need DECPMAX) */
#include "decCommon.c"	      /* non-arithmetic decFloat routines */
#include "decBasic.c"	      /* basic formats routines */


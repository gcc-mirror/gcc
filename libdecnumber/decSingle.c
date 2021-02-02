/* decSingle module for the decNumber C Library.
   Copyright (C) 2007-2021 Free Software Foundation, Inc.
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
/* decSingle.c -- decSingle operations module			      */
/* ------------------------------------------------------------------ */

#include "decContext.h"       /* public includes */
#include "decSingle.h"	      /* public includes */

/* Constant mappings for shared code */
#define DECPMAX     DECSINGLE_Pmax
#define DECEMIN     DECSINGLE_Emin
#define DECEMAX     DECSINGLE_Emax
#define DECEMAXD    DECSINGLE_EmaxD
#define DECBYTES    DECSINGLE_Bytes
#define DECSTRING   DECSINGLE_String
#define DECECONL    DECSINGLE_EconL
#define DECBIAS     DECSINGLE_Bias
#define DECLETS     DECSINGLE_Declets
#define DECQTINY    (-DECSINGLE_Bias)
/* parameters of next-wider format */
#define DECWBYTES   DECDOUBLE_Bytes
#define DECWPMAX    DECDOUBLE_Pmax
#define DECWECONL   DECDOUBLE_EconL
#define DECWBIAS    DECDOUBLE_Bias

/* Type and function mappings for shared code */
#define decFloat		   decSingle	  /* Type name */
#define decFloatWider		   decDouble	  /* Type name */

/* Utility (binary results, extractors, etc.) */
#define decFloatFromBCD 	   decSingleFromBCD
#define decFloatFromPacked	   decSingleFromPacked
#define decFloatFromPackedChecked  decSingleFromPackedChecked
#define decFloatFromString	   decSingleFromString
#define decFloatFromWider	   decSingleFromWider
#define decFloatGetCoefficient	   decSingleGetCoefficient
#define decFloatGetExponent	   decSingleGetExponent
#define decFloatSetCoefficient	   decSingleSetCoefficient
#define decFloatSetExponent	   decSingleSetExponent
#define decFloatShow		   decSingleShow
#define decFloatToBCD		   decSingleToBCD
#define decFloatToEngString	   decSingleToEngString
#define decFloatToPacked	   decSingleToPacked
#define decFloatToString	   decSingleToString
#define decFloatToWider 	   decSingleToWider
#define decFloatZero		   decSingleZero

/* Non-computational */
#define decFloatRadix		   decSingleRadix
#define decFloatVersion 	   decSingleVersion

#include "decNumberLocal.h"   /* local includes (need DECPMAX) */
#include "decCommon.c"	      /* non-basic decFloat routines */
/* [Do not include decBasic.c for decimal32] */


/* Decimal 64-bit format module header for the decNumber C Library.
   Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
/* Decimal 64-bit format module header				      */
/* ------------------------------------------------------------------ */

#if !defined(DECIMAL64)
  #define DECIMAL64
  #define DEC64NAME	"decimal64"		      /* Short name   */
  #define DEC64FULLNAME "Decimal 64-bit Number"       /* Verbose name */
  #define DEC64AUTHOR	"Mike Cowlishaw"	      /* Who to blame */


  /* parameters for decimal64s					      */
  #define DECIMAL64_Bytes  8		/* length		      */
  #define DECIMAL64_Pmax   16		/* maximum precision (digits) */
  #define DECIMAL64_Emax   384		/* maximum adjusted exponent  */
  #define DECIMAL64_Emin  -383		/* minimum adjusted exponent  */
  #define DECIMAL64_Bias   398		/* bias for the exponent      */
  #define DECIMAL64_String 24		/* maximum string length, +1  */
  #define DECIMAL64_EconL  8		/* exp. continuation length   */
  /* highest biased exponent (Elimit-1) 			      */
  #define DECIMAL64_Ehigh  (DECIMAL64_Emax+DECIMAL64_Bias-DECIMAL64_Pmax+1)

  /* check enough digits, if pre-defined			      */
  #if defined(DECNUMDIGITS)
    #if (DECNUMDIGITS<DECIMAL64_Pmax)
      #error decimal64.h needs pre-defined DECNUMDIGITS>=16 for safe use
    #endif
  #endif


  #ifndef DECNUMDIGITS
    #define DECNUMDIGITS DECIMAL64_Pmax /* size if not already defined*/
  #endif
  #ifndef DECNUMBER
    #include "decNumber.h"		/* context and number library */
  #endif

  /* Decimal 64-bit type, accessible by bytes			      */
  typedef struct {
    uint8_t bytes[DECIMAL64_Bytes];	/* decimal64: 1, 5, 8, 50 bits*/
    } decimal64;

  /* special values [top byte excluding sign bit; last two bits are   */
  /* don't-care for Infinity on input, last bit don't-care for NaN]   */
  #if !defined(DECIMAL_NaN)
    #define DECIMAL_NaN     0x7c	/* 0 11111 00 NaN	      */
    #define DECIMAL_sNaN    0x7e	/* 0 11111 10 sNaN	      */
    #define DECIMAL_Inf     0x78	/* 0 11110 00 Infinity	      */
  #endif

  /* ---------------------------------------------------------------- */
  /* Routines							      */
  /* ---------------------------------------------------------------- */

#include "decimal64Symbols.h"

  #ifdef __cplusplus
  extern "C" {
  #endif

  /* String conversions 					      */
  decimal64 * decimal64FromString(decimal64 *, const char *, decContext *);
  char * decimal64ToString(const decimal64 *, char *);
  char * decimal64ToEngString(const decimal64 *, char *);

  /* decNumber conversions					      */
  decimal64 * decimal64FromNumber(decimal64 *, const decNumber *,
				  decContext *);
  decNumber * decimal64ToNumber(const decimal64 *, decNumber *);

  /* Format-dependent utilities 				      */
  uint32_t    decimal64IsCanonical(const decimal64 *);
  decimal64 * decimal64Canonical(decimal64 *, const decimal64 *);

  #ifdef __cplusplus
  }
  #endif

#endif

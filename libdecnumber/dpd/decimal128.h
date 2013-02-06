/* Decimal 128-bit format module header for the decNumber C Library.
   Copyright (C) 2005-2013 Free Software Foundation, Inc.
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
/* Decimal 128-bit format module header 			      */
/* ------------------------------------------------------------------ */

#if !defined(DECIMAL128)
  #define DECIMAL128
  #define DEC128NAME	 "decimal128"		      /* Short name   */
  #define DEC128FULLNAME "Decimal 128-bit Number"     /* Verbose name */
  #define DEC128AUTHOR	 "Mike Cowlishaw"	      /* Who to blame */

  /* parameters for decimal128s */
  #define DECIMAL128_Bytes  16		/* length		      */
  #define DECIMAL128_Pmax   34		/* maximum precision (digits) */
  #define DECIMAL128_Emax   6144	/* maximum adjusted exponent  */
  #define DECIMAL128_Emin  -6143	/* minimum adjusted exponent  */
  #define DECIMAL128_Bias   6176	/* bias for the exponent      */
  #define DECIMAL128_String 43		/* maximum string length, +1  */
  #define DECIMAL128_EconL  12		/* exp. continuation length   */
  /* highest biased exponent (Elimit-1) 			      */
  #define DECIMAL128_Ehigh  (DECIMAL128_Emax+DECIMAL128_Bias-DECIMAL128_Pmax+1)

  /* check enough digits, if pre-defined			      */
  #if defined(DECNUMDIGITS)
    #if (DECNUMDIGITS<DECIMAL128_Pmax)
      #error decimal128.h needs pre-defined DECNUMDIGITS>=34 for safe use
    #endif
  #endif

  #ifndef DECNUMDIGITS
    #define DECNUMDIGITS DECIMAL128_Pmax /* size if not already defined*/
  #endif
  #ifndef DECNUMBER
    #include "decNumber.h"		/* context and number library */
  #endif

  /* Decimal 128-bit type, accessible by bytes			      */
  typedef struct {
    uint8_t bytes[DECIMAL128_Bytes]; /* decimal128: 1, 5, 12, 110 bits*/
    } decimal128;

  /* special values [top byte excluding sign bit; last two bits are   */
  /* don't-care for Infinity on input, last bit don't-care for NaN]   */
  #if !defined(DECIMAL_NaN)
    #define DECIMAL_NaN     0x7c	/* 0 11111 00 NaN	      */
    #define DECIMAL_sNaN    0x7e	/* 0 11111 10 sNaN	      */
    #define DECIMAL_Inf     0x78	/* 0 11110 00 Infinity	      */
  #endif

#include "decimal128Local.h"

  /* ---------------------------------------------------------------- */
  /* Routines							      */
  /* ---------------------------------------------------------------- */

#include "decimal128Symbols.h"

  #ifdef __cplusplus
  extern "C" {
  #endif

  /* String conversions 					      */
  decimal128 * decimal128FromString(decimal128 *, const char *, decContext *);
  char * decimal128ToString(const decimal128 *, char *);
  char * decimal128ToEngString(const decimal128 *, char *);

  /* decNumber conversions					      */
  decimal128 * decimal128FromNumber(decimal128 *, const decNumber *,
				    decContext *);
  decNumber * decimal128ToNumber(const decimal128 *, decNumber *);

  /* Format-dependent utilities 				      */
  uint32_t    decimal128IsCanonical(const decimal128 *);
  decimal128 * decimal128Canonical(decimal128 *, const decimal128 *);

  #ifdef __cplusplus
  }
  #endif

#endif

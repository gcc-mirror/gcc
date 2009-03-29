/* decSingle module header for the decNumber C Library.
   Copyright (C) 2005 Free Software Foundation, Inc.
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
/* decSingle.h -- Decimal 32-bit format module header		      */
/* ------------------------------------------------------------------ */
/* Please see decFloats.h for an overview and documentation details.  */
/* ------------------------------------------------------------------ */

#if !defined(DECSINGLE)
  #define DECSINGLE

  #define DECSINGLENAME	      "decSingle"	      /* Short name   */
  #define DECSINGLETITLE      "Decimal 32-bit datum"  /* Verbose name */
  #define DECSINGLEAUTHOR     "Mike Cowlishaw"	      /* Who to blame */

  /* parameters for decSingles */
  #define DECSINGLE_Bytes    4	   /* length			      */
  #define DECSINGLE_Pmax     7	   /* maximum precision (digits)      */
  #define DECSINGLE_Emin   -95	   /* minimum adjusted exponent	      */
  #define DECSINGLE_Emax    96	   /* maximum adjusted exponent	      */
  #define DECSINGLE_EmaxD    3	   /* maximum exponent digits	      */
  #define DECSINGLE_Bias   101	   /* bias for the exponent	      */
  #define DECSINGLE_String  16	   /* maximum string length, +1	      */
  #define DECSINGLE_EconL    6	   /* exponent continuation length    */
  #define DECSINGLE_Declets  2	   /* count of declets		      */
  /* highest biased exponent (Elimit-1) */
  #define DECSINGLE_Ehigh (DECSINGLE_Emax + DECSINGLE_Bias - (DECSINGLE_Pmax-1))

  /* Required includes						      */
  #include "decContext.h"
  #include "decQuad.h"
  #include "decDouble.h"

  /* The decSingle decimal 32-bit type, accessible by various types */
  typedef union {
    uint8_t bytes[DECSINGLE_Bytes];	/* fields: 1, 5, 6, 20 bits */
    uint16_t shorts[DECSINGLE_Bytes/2];
    uint32_t words[DECSINGLE_Bytes/4];
    } decSingle;

  /* ---------------------------------------------------------------- */
  /* Routines -- implemented as decFloat routines in common files     */
  /* ---------------------------------------------------------------- */

  #include "decSingleSymbols.h"

  /* Utilities (binary argument(s) or result, extractors, etc.) */
  extern decSingle * decSingleFromBCD(decSingle *, int32_t, const uint8_t *, int32_t);
  extern decSingle * decSingleFromPacked(decSingle *, int32_t, const uint8_t *);
  extern decSingle * decSingleFromString(decSingle *, const char *, decContext *);
  extern decSingle * decSingleFromWider(decSingle *, const decDouble *, decContext *);
  extern int32_t     decSingleGetCoefficient(const decSingle *, uint8_t *);
  extern int32_t     decSingleGetExponent(const decSingle *);
  extern decSingle * decSingleSetCoefficient(decSingle *, const uint8_t *, int32_t);
  extern decSingle * decSingleSetExponent(decSingle *, decContext *, int32_t);
  extern void	     decSingleShow(const decSingle *, const char *);
  extern int32_t     decSingleToBCD(const decSingle *, int32_t *, uint8_t *);
  extern char	   * decSingleToEngString(const decSingle *, char *);
  extern int32_t     decSingleToPacked(const decSingle *, int32_t *, uint8_t *);
  extern char	   * decSingleToString(const decSingle *, char *);
  extern decDouble * decSingleToWider(const decSingle *, decDouble *);
  extern decSingle * decSingleZero(decSingle *);

  /* (No Arithmetic routines for decSingle) */

  /* Non-computational */
  extern uint32_t     decSingleRadix(const decSingle *);
  extern const char * decSingleVersion(void);

  /* decNumber conversions; these are implemented as macros so as not  */
  /* to force a dependency on decimal32 and decNumber in decSingle.    */
  #define decSingleToNumber(dq, dn) decimal32ToNumber((decimal32 *)(dq), dn)
  #define decSingleFromNumber(dq, dn, set) (decSingle *)decimal32FromNumber((decimal32 *)(dq), dn, set)

#endif

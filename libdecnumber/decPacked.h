/* Packed decimal conversion module header for the decNumber C Library.
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
/* Packed Decimal conversion module header			      */
/* ------------------------------------------------------------------ */

#if !defined(DECPACKED)
  #define DECPACKED
  #define DECPNAME     "decPacked"			/* Short name */
  #define DECPFULLNAME "Packed Decimal conversions"   /* Verbose name */
  #define DECPAUTHOR   "Mike Cowlishaw"		      /* Who to blame */

  #define DECPACKED_DefP 32		/* default precision	      */

  #ifndef  DECNUMDIGITS
    #define DECNUMDIGITS DECPACKED_DefP /* size if not already defined*/
  #endif
  #include "decNumber.h"		/* context and number library */

  /* Sign nibble constants					      */
  #if !defined(DECPPLUSALT)
    #define DECPPLUSALT	 0x0A /* alternate plus	 nibble		      */
    #define DECPMINUSALT 0x0B /* alternate minus nibble		      */
    #define DECPPLUS	 0x0C /* preferred plus	 nibble		      */
    #define DECPMINUS	 0x0D /* preferred minus nibble		      */
    #define DECPPLUSALT2 0x0E /* alternate plus	 nibble		      */
    #define DECPUNSIGNED 0x0F /* alternate plus	 nibble (unsigned)    */
  #endif

  /* ---------------------------------------------------------------- */
  /* decPacked public routines					      */
  /* ---------------------------------------------------------------- */

  #include "decPackedSymbols.h"

  /* Conversions						      */
  uint8_t * decPackedFromNumber(uint8_t *, int32_t, int32_t *,
				const decNumber *);
  decNumber * decPackedToNumber(const uint8_t *, int32_t, const int32_t *,
				decNumber *);

#endif

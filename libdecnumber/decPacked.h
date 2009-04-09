/* Packed decimal conversion module header for the decNumber C Library.
   Copyright (C) 2007, 2009 Free Software Foundation, Inc.
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
/* Packed Decimal conversion module header			      */
/* ------------------------------------------------------------------ */

#if !defined(DECPACKED)
  #define DECPACKED
  #define DECPNAME     "decPacked"			/* Short name */
  #define DECPFULLNAME "Packed Decimal conversions"   /* Verbose name */
  #define DECPAUTHOR   "Mike Cowlishaw" 	      /* Who to blame */

  #define DECPACKED_DefP 32		/* default precision	      */

  #ifndef  DECNUMDIGITS
    #define DECNUMDIGITS DECPACKED_DefP /* size if not already defined*/
  #endif
  #include "decNumber.h"		/* context and number library */

  /* Sign nibble constants					      */
  #if !defined(DECPPLUSALT)
    #define DECPPLUSALT  0x0A /* alternate plus  nibble 	      */
    #define DECPMINUSALT 0x0B /* alternate minus nibble 	      */
    #define DECPPLUS	 0x0C /* preferred plus  nibble 	      */
    #define DECPMINUS	 0x0D /* preferred minus nibble 	      */
    #define DECPPLUSALT2 0x0E /* alternate plus  nibble 	      */
    #define DECPUNSIGNED 0x0F /* alternate plus  nibble (unsigned)    */
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

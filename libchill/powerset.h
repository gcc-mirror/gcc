/* Common macros for POWERSET runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
   Author: Wilfried Moser, et al

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifndef _POWERSET_H
#define _POWERSET_H

#define USE_CHARS

#ifdef USE_CHARS

#define SET_WORD unsigned char
#define SET_CHAR  unsigned char
#define SET_SHORT unsigned char

#else

#ifndef SET_WORD
#define SET_WORD unsigned int
#endif
#define SET_CHAR  unsigned char
#define SET_SHORT unsigned short
#endif

#define SET_WORD_SIZE (sizeof (char) * sizeof (SET_WORD))
#define SET_SHORT_SIZE (sizeof (char) * sizeof (SET_SHORT))
#define SET_CHAR_SIZE sizeof (char)

/* Powersets and bit strings are stored as arrays of SET_WORD.
   if they are a word or longer.  Powersets and bit strings whic
   fit in a byte or short are stored that way by the compiler.

   The order of the bits follows native bit order:
   If BITS_BIG_ENDIAN, bit 0 is the most significant bit (i.e. 0x80..00);
   otherwise, bit 0 is the least significant bit (i.e. 0x1).

   MASK_UNUSED_BITS masks out unused bits in powersets and bitstrings.
   GET_BIT_IN_WORD(W,B) yields 1 (or 0) if the B'th bit if W is set (cleared).
*/

#if BITS_BIG_ENDIAN
#define GET_BIT_IN_WORD(w,b) (((w) >> (SET_WORD_SIZE - 1 - (b))) & 1)
#define GET_BIT_IN_SHORT(w,b) (((w) >> (SET_SHORT_SIZE - 1 - (b))) & 1)
#define GET_BIT_IN_CHAR(w,b) (((w) >> (SET_CHAR_SIZE - 1 - (b))) & 1)

#define SET_BIT_IN_WORD(w,b) ((w) |= 1 << ((SET_WORD_SIZE) - 1 - (b)))
#define SET_BIT_IN_SHORT(w,b) ((w) |= 1 << ((SET_SHORT_SIZE) - 1 - (b)))
#define SET_BIT_IN_CHAR(w,b) ((w) |= 1 << ((SET_CHAR_SIZE) - 1 - (b)))

#define CLEAR_BIT_IN_WORD(w,b) ((w) &= ~(1 << ((SET_WORD_SIZE) - 1 - (b))))
#define CLEAR_BIT_IN_SHORT(w,b) ((w) &= ~(1 << ((SET_SHORT_SIZE) - 1 - (b))))
#define CLEAR_BIT_IN_CHAR(w,b) ((w) &= ~(1 << ((SET_CHAR_SIZE) - 1 - (b))))
#define MASK_UNUSED_WORD_BITS(p,b)		   \
{ if (b) *(p) &= (~0) << (SET_WORD_SIZE - (b)); }
#define MASK_UNUSED_SHORT_BITS(p,b)		   \
{ if (b) *(p) &= (~0) << (SET_SHORT_SIZE - (b)); }
#define MASK_UNUSED_CHAR_BITS(p,b)                 \
{ if (b) *(p) &= (~0) << (SET_CHAR_SIZE - (b)); }

#else /* !BITS_BIG_ENDIAN */

#define GET_BIT_IN_WORD(w,b) (((w) >> (b)) & 1)
#define GET_BIT_IN_SHORT(w,b) GET_BIT_IN_WORD(w,b)
#define GET_BIT_IN_CHAR(w,b) GET_BIT_IN_WORD(w,b)

#define SET_BIT_IN_WORD(w,b) ((w) |= 1 << (b))
#define SET_BIT_IN_SHORT(w,b) SET_BIT_IN_WORD(w,b)
#define SET_BIT_IN_CHAR(w,b) SET_BIT_IN_WORD(w,b)

#define CLEAR_BIT_IN_WORD(w,b) ((w) &= ~(1 << (b)))
#define CLEAR_BIT_IN_SHORT(w,b) CLEAR_BIT_IN_WORD(w,b)
#define CLEAR_BIT_IN_CHAR(w,b) CLEAR_BIT_IN_WORD(w,b)

#define MASK_UNUSED_WORD_BITS(p,b)  \
{ if (b) *(p) &= ~((~0) << (b)); }
#define MASK_UNUSED_SHORT_BITS(p,b) MASK_UNUSED_WORD_BITS(p,b)
#define MASK_UNUSED_CHAR_BITS(p,b) MASK_UNUSED_WORD_BITS(p,b)

#endif


/* Number of words needed for a bitstring/powerset of size BITLENGTH.
   This definition handles the (BITLENGTH==0) by yielding 0. */

#define BITS_TO_WORDS(BITLENGTH) \
  (((BITLENGTH) + (SET_WORD_SIZE-1)) / SET_WORD_SIZE)
#define BITS_TO_CHARS(BITLENGTH) \
  (((BITLENGTH) + (SET_CHAR_SIZE-1)) / SET_CHAR_SIZE)

#endif

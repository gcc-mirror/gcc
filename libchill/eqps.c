/* Implement POWERSET runtime actions for CHILL.
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

#define __CHILL_LIB__

#include <stdio.h>
#include "powerset.h"

/*
 * function __eqpowerset
 *
 * parameters:
 *	left		left powerset
 *	right		right powerset
 *	bitlength	length of powerset in bits
 *
 * returns:
 *    1 if powersets are equal, bit for bit
 *
 * exceptions:
 *  none
 *
 * abstract:
 *  compares two powersets for equality
 *
 */
int
__eqpowerset (left, right, bitlength)
     SET_WORD *left;
     SET_WORD *right;
     unsigned long bitlength;
{
#ifndef USE_CHARS
  if (bitlength <= SET_CHAR_SIZE)
    {
      SET_CHAR c = *(SET_CHAR *)left ^ *(SET_CHAR *)right;
      MASK_UNUSED_CHAR_BITS (&c, bitlength);
      return (c == 0) ? 1 : 0;
    }
  else if (bitlength <= SET_SHORT_SIZE)
    {
      SET_SHORT c = *(SET_SHORT *)left ^ *(SET_SHORT *)right;
      MASK_UNUSED_SHORT_BITS (&c, bitlength);
      return (c == 0) ? 1 : 0;
    }
  else if (bitlength <= SET_WORD_SIZE)
    {
      SET_WORD c = *(SET_WORD *)left ^ *(SET_WORD *)right;
      MASK_UNUSED_WORD_BITS (&c, bitlength % SET_WORD_SIZE);
      return (c == 0) ? 1 : 0;
    }
  else
#endif
    {
      SET_WORD c;
      register unsigned long i;
      unsigned long len = bitlength / SET_WORD_SIZE;

      for (i = 0; i < len; i++) /* a word-oriented memcmp */
	if (left[i] != right[i])
	  return 0;
      /* do the last (possibly partial) word */
      bitlength %= SET_WORD_SIZE;
      if (bitlength == 0)
	return 1;
      c = left[i] ^ right[i];
      MASK_UNUSED_WORD_BITS (&c, bitlength);
      return (c == 0) ? 1 : 0;
    }
}

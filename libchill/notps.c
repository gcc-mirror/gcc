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
 * function __notpowerset
 *
 * parameters:
 *	out		output powerset
 *	left		input powerset
 *	bitlength	length of powerset in bits
 *
 * returns:
 *	void
 *
 * exceptions:
 *  none
 *
 * abstract:
 *
 */

void
__notpowerset (out, left, bitlength)
     SET_WORD      *out;
     SET_WORD      *left;
     unsigned long  bitlength;
{
  if (bitlength <= SET_CHAR_SIZE)
    {
      *((SET_CHAR *)out) = ~ (*((SET_CHAR *)left));
#if 0
      SET_CHAR tmp;
      tmp = *((SET_CHAR *)left);
      tmp = ~ tmp;
      *((SET_CHAR *)out) = tmp;

      MASK_UNUSED_CHAR_BITS((SET_CHAR *)out, bitlength);
      *((SET_CHAR *)out) = ~ *((SET_CHAR *)left);
      MASK_UNUSED_CHAR_BITS((SET_CHAR *)out, bitlength);
      *((SET_CHAR *)out) = (~(0)) ^  (*((SET_CHAR *)left));
      MASK_UNUSED_CHAR_BITS((SET_CHAR *)out, bitlength);
#endif
    }
  else if (bitlength <= SET_SHORT_SIZE)
    {
      *((SET_SHORT *)out) = ~  (*((SET_SHORT *)left));
      MASK_UNUSED_SHORT_BITS((SET_SHORT *)out, bitlength);
    }
  else
    {
      unsigned long len = BITS_TO_WORDS(bitlength);
      register unsigned long i;
    
      for (i = 0; i < len; i++)
	out[i] = ~ left[i];
      MASK_UNUSED_WORD_BITS((out + len - 1), bitlength % SET_WORD_SIZE);
    }
}

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
 * function __cardpowerset
 *
 * parameters:
 *	ps		powerset
 *	bitlength	length of powerset
 *
 * returns:
 *	long		number of set bits
 *
 * exceptions:
 *  none
 *
 * abstract:
 *  returns the number of set bit's in a powerset
 *
 */

/* bit_count[I] is number of '1' bits in I. */
static
const unsigned char __four_bit_count[16] = {
    0, 1, 1, 2,
    1, 2, 2, 3,
    1, 2, 2, 3,
    2, 3, 3, 4 };

long
__cardpowerset (ps, bitlength)
     SET_WORD      *ps;
     unsigned long  bitlength;
{
  unsigned long	count = 0;
  if (bitlength <= SET_CHAR_SIZE)
    {
      register SET_CHAR	c = *((SET_CHAR *)ps);
      /* count 4 bits at a time. */
      while (c > 0)
	{
	  count += __four_bit_count[c & 15];
	  c >>= 4;
	}
      return count;
    }
  else if (bitlength <= SET_SHORT_SIZE)
    {
      register SET_SHORT c = *((SET_SHORT *)ps);
      /* count 4 bits at a time. */
      while (c > 0)
	{
	  count += __four_bit_count[c & 15];
	  c >>= 4;
	}
      return count;
    }
  else
    {
      register SET_WORD	*p = ps;
      SET_WORD *endp = p + BITS_TO_WORDS(bitlength);
    
      while (p < endp)
	{
	  register SET_WORD c = *p++;
	  /* count 4 bits at a time. */
	  while (c > 0)
	    {
	      count += __four_bit_count[c & 15];
	      c >>= 4;
	    }
	}
      return (count);
    }
}

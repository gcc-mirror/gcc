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
 * function __ffsetclrpowerset
 *
 * parameters:
 *	ps		powerset
 *	bitlength	length of powerset
 *
 * returns:
 *	int		-1 .. nothing found
 *			>=0 .. index of first true bit found
 * exceptions:
 *  none
 */

int
__ffsetclrpowerset (ps, bitlength, first_bit)
     SET_WORD      *ps;
     unsigned long  bitlength;
     int first_bit;
{
  register int bitno;

  if (first_bit >= bitlength)
    return -1;

#ifndef USE_CHARS
  if (bitlength <= SET_CHAR_SIZE)
    {
      for (bitno = first_bit; bitno < bitlength; bitno++)
	if (GET_BIT_IN_CHAR (*((SET_CHAR *)ps), bitno))
	  break;
      return bitno == bitlength ? -1 : bitno;
    }
  else if (bitlength <= SET_SHORT_SIZE)
    {
      for (bitno = first_bit; bitno < bitlength; bitno++)
	if (GET_BIT_IN_SHORT (*((SET_SHORT *)ps), bitno))
	  break;
      return bitno == bitlength ? -1 : bitno;
    }
  else
#endif
    {
      unsigned int words_to_skip = (unsigned) first_bit / SET_WORD_SIZE;
      unsigned long cnt = words_to_skip * SET_WORD_SIZE;
      SET_WORD	*p = ps + words_to_skip;
      SET_WORD	*endp = ps + BITS_TO_WORDS(bitlength);
      SET_WORD	c;
      first_bit = (unsigned) first_bit % (unsigned) SET_WORD_SIZE;

      c = *p++;
      if (c)
	{
	  for (bitno = first_bit; bitno < SET_WORD_SIZE; bitno++)
	    if (GET_BIT_IN_WORD(c, bitno))
	      goto found;
	}
      cnt += SET_WORD_SIZE;

      while (p < endp)
	{
	  if ((c = *p++))
	    {
	      /* found a bit set .. calculate which */
	      for (bitno = 0; bitno < SET_WORD_SIZE; bitno++)
		if (GET_BIT_IN_WORD(c, bitno))
		  goto found;
	    }
	  cnt += SET_WORD_SIZE;
	}
      return -1;
    found:
      bitno += cnt;
      return bitno >= bitlength ? -1 : bitno;
    }
}

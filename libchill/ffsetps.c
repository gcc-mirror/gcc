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

extern void __cause_ex1 (char *exname, char *file, int lineno);

/*
 * function __ffsetpowerset
 *
 * parameters:
 *	ps		powerset
 *	bitlength	length of powerset
 *      minval          set low bound
 *      filename        caller's file name
 *      lineno          caller's line number
 *
 * returns:
 *	int		enum value of lowest member of powerset
 *
 * exceptions:
 *      "empty" if the set is empty
 *
 * abstract:
 *  Find first bit set in a powerset and return the corresponding value.
 *
 */
long
__ffsetpowerset (ps, bitlength, minval, filename, lineno)
     SET_WORD      *ps;
     unsigned long  bitlength;
     long           minval;
     char          *filename;
     int            lineno;
{
  unsigned long bitno;

  if (bitlength <= SET_CHAR_SIZE)
    {
      SET_CHAR cset = *((SET_CHAR *)ps);
      if (cset != 0)
	{
	  /* found a bit set .. calculate which */
	  for (bitno = 0; bitno < SET_CHAR_SIZE; bitno++)
	    if (GET_BIT_IN_CHAR (cset, bitno))
	      break;
	  /* return its index */
	  return bitno + minval;
	}
    }
  else if (bitlength <= SET_SHORT_SIZE)
    {
      SET_SHORT sset = *((SET_SHORT *)ps);
      if (sset != 0)
	{
	  /* found a bit set .. calculate which */
	  for (bitno = 0; bitno < SET_SHORT_SIZE; bitno++)
	    if (GET_BIT_IN_SHORT (sset, bitno))
	      break;
	  /* return its index */
	  return bitno + minval;
	}
    }
  else /* set composed of array of one or more WORDs */
    {
      SET_WORD *p    = ps;
      SET_WORD *endp = ps + BITS_TO_WORDS(bitlength);
      unsigned long    cnt;
      
      /* FIXME: bitorder problems? */
      for (cnt = 0; p < endp; p++, cnt += SET_WORD_SIZE)
	{
	  SET_WORD c = *p;

	  if (c)
	    {
	      /* found a bit set .. calculate which */
	      for (bitno = 0; bitno < SET_WORD_SIZE; bitno++)
		if (GET_BIT_IN_WORD (c, bitno))
		  break;
	      /* return its index */
	      return cnt + bitno + minval;
	    }
	}
    }
  /* no bits found - raise exception */
  __cause_ex1 ("empty", filename, lineno);
}

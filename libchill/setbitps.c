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
 * function __setbitpowerset
 *
 * parameters:
 *	set		destination set
 *	bitlength	length of powerset in bits
 *      minval          lowest valid set value
 *      bitno           bit number within set
 *      new_value       zero or one - (new bit value)
 *
 * returns:
 *	int		1 .. found
 *			0 .. not found
 *
 * exceptions:
 *  rangefail
 *
 * abstract:
 *  checks if a given value is included in a powerset
 *
 */
void
__setbitpowerset (powerset, bitlength, minval, bitno, new_value, filename, lineno)
     SET_WORD      *powerset;
     unsigned long  bitlength;
     long           minval;
     long	    bitno;
     char	    new_value; /* booleans are represented as 8 bit value */
     char *	    filename;
     int	    lineno;
{
  if (powerset == NULL
      || bitno < minval 
      || (bitno - minval) >= bitlength)
    __cause_ex1 ("rangefail", filename, lineno);

  bitno -= minval;
  if (bitlength <= SET_CHAR_SIZE)
    {
      if (new_value & 1)
	SET_BIT_IN_CHAR (*((SET_CHAR *)powerset), bitno);
      else
	CLEAR_BIT_IN_CHAR (*((SET_CHAR *)powerset), bitno);
    }
  else if (bitlength <= SET_SHORT_SIZE)
    {
      if (new_value & 1)
	SET_BIT_IN_SHORT (*((SET_SHORT *)powerset), bitno);
      else
	CLEAR_BIT_IN_SHORT (*((SET_SHORT *)powerset), bitno);
    }
  else
    {
      powerset += (bitno/SET_WORD_SIZE);
      bitno %= SET_WORD_SIZE;
      if (new_value & 1)
	SET_BIT_IN_WORD (*powerset, bitno);
      else
	CLEAR_BIT_IN_WORD (*powerset, bitno);
    }
}

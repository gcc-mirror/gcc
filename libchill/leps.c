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
 * function __lepowerset
 *
 * parameters:
 *	left		powerset
 *	right		powerset
 *	bitlength	length of powerset
 *
 * returns:
 *	int		1 .. left is included in right
 *			0 .. not
 *
 * abstract:
 *  check if one powerset is included in another
 *
 */
int
__lepowerset (left, right, bitlength)
     SET_WORD      *left;
     SET_WORD      *right;
     unsigned long  bitlength;
{
  if (bitlength <= SET_CHAR_SIZE)
    {
      if ((*((SET_CHAR *)left) & *((SET_CHAR *)right))
	  != *((SET_CHAR *)left))
	return 0;
      return 1;
    }
  else if (bitlength <= SET_SHORT_SIZE)
    {
      if ((*((SET_SHORT *)left) & *((SET_SHORT *)right))
	  != *((SET_SHORT *)left))
	return 0;
      return 1;
    }
  else
    {
      SET_WORD *endp = left + BITS_TO_WORDS(bitlength);
    
      while (left < endp)
	{
	  if ((*right & *left) != *left)
	    return 0;
	  left++;
	  right++;
	}
      return 1;
    }
}

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
 * function __powerset_copy
 *    This is more general than __psslice, since it
 *    can be told where in the destination powerset (DOFFSET
 *    parameter) to start storing the slice.
 *
 * parameters:
 *      dps             dest powerset
 *      dbl             destination bit length
 *      doffset         offset bit number (zero origin)
 *	sps		sourcepowerset
 *	sbl     	source powerset length in bits
 *      start           starting bit number
 *      end             ending bit number
 *
 * exceptions:
 *  none
 *
 * abstract:
 *  Extract into a powerset a slice of another powerset.
 *
 */
void
__pscpy (dps, dbl, doffset, sps, sbl, start, length)
     SET_WORD      *dps;
     unsigned long  dbl;
     unsigned long  doffset;
     const SET_WORD*sps;
     unsigned long  sbl;
     unsigned long  start;
     unsigned long  length;
{
  unsigned long end = start + length - 1;
  unsigned long src, dst;

  /* assert end >= start;
     assert end - start + 1 <= dbl;
     assert "the sets don't overlap in memory" */

  /* assert doffset >= 0 and < dbl */

  for (src = start, dst = doffset; src <= end; src++, dst++)
    {
      char tmp;

      if (sbl <= SET_CHAR_SIZE)                /* fetch a bit */
	tmp = GET_BIT_IN_CHAR (*((SET_CHAR *)sps), src);
      else if (sbl <= SET_SHORT_SIZE)
	tmp = GET_BIT_IN_SHORT (*((SET_SHORT *)sps), src);
      else
	tmp = GET_BIT_IN_WORD (sps[src / SET_WORD_SIZE], src % SET_WORD_SIZE);

      if (tmp & 1)
	{
	  if (dbl <= SET_CHAR_SIZE)            /* store a 1-bit */
	    SET_BIT_IN_CHAR (*((SET_CHAR *)dps), dst);
	  else if (dbl <= SET_SHORT_SIZE)
	    SET_BIT_IN_SHORT (*((SET_SHORT *)dps), dst);
	  else
	    SET_BIT_IN_WORD (dps[dst / SET_WORD_SIZE], dst % SET_WORD_SIZE);
	}
      else
	{
	  if (dbl <= SET_CHAR_SIZE)            /* store a 0-bit */
	    CLEAR_BIT_IN_CHAR (*((SET_CHAR *)dps), dst);
	  else if (dbl <= SET_SHORT_SIZE)
	    CLEAR_BIT_IN_SHORT (*((SET_SHORT *)dps), dst);
	  else
	    CLEAR_BIT_IN_WORD (dps[dst / SET_WORD_SIZE], dst % SET_WORD_SIZE);
	}
    }
  if (dbl <= SET_CHAR_SIZE)         /* clear unused bits in output bitstring */
    {
      MASK_UNUSED_CHAR_BITS ((SET_CHAR *)dps, dbl);
    }
  else if (dbl <= SET_SHORT_SIZE)
    {
      MASK_UNUSED_SHORT_BITS ((SET_SHORT *)dps, dbl);
    }
  else
    {
      MASK_UNUSED_WORD_BITS ((SET_WORD *)(dps + (dbl/SET_WORD_SIZE)), 
			     dbl % SET_WORD_SIZE);
    }
}

/* Implement powerset-related runtime actions for CHILL.
   Copyright (C) 1992, 93, 1994 Free Software Foundation, Inc.
   Author: Bill Cox

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

#include "powerset.h"

extern void cause_exception (char *exname, char *file, int lineno);

/*
 * function __concatps
 *
 * parameters:
 *     OUT      - pointer to output PS
 *     LEFT     - pointer to left PS
 *     LEFTLEN  - length of left PS in bits
 *     RIGHT    - pointer to right PS
 *     RIGHTLEN - length of right PS in bits
 *
 * returns:
 *     void
 *
 * exceptions:
 *     none
 *
 * abstract:
 *     concatenates two powersets into the output powerset.
 *
 */

extern void
__pscpy (SET_WORD      *dps,
	 unsigned long  dbl,
	 unsigned long  doffset,
	 SET_WORD      *sps,
	 unsigned long  sbl,
	 unsigned long  start,
	 unsigned long  length);

void
__concatps (out, left, leftlen, right, rightlen)
     SET_WORD      *out;
     SET_WORD      *left;
     unsigned long  leftlen;
     SET_WORD      *right;
     unsigned long  rightlen;
{
  /* allocated sizes for each set involved */
  unsigned long outall, leftall, rightall;

  if (!out)
    {
      /* FIXME: cause an exception */
    }
  else if (leftlen == 0 || !left)
    {
      if (rightlen == 0 || !right)
	return;               /* no work to do */
      __pscpy (out, rightlen, (unsigned long)0,
	       right, rightlen, (unsigned long)0, rightlen);
    }
  else if (rightlen == 0 || !right)
    {
      if (leftlen == 0 || !left)
	return;               /* no work to do */
      __pscpy (out, leftlen, (unsigned long)0,
	       left, leftlen, (unsigned long)0, leftlen);
    }
  /* copy the left powerset into bits 0..leftlen - 1 */
  __pscpy (out, leftlen + rightlen, (unsigned long)0,
	   left, leftlen, (unsigned long)0, leftlen);

  /* copy the right powerset into bits leftlen..leftlen+rightlen-1 */
  __pscpy (out, leftlen + rightlen, leftlen,
	   right, rightlen, (unsigned long)0, rightlen);
}

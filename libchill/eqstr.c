/* Implement string-related runtime actions for CHILL.
   Copyright (C) 1992,1993 Free Software Foundation, Inc.
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

extern void cause_exception (char *exname, char *file, int lineno);

/*
 * function __eqstring
 *
 * parameters:
 *     S1 - pointer to left string
 *     LEN1 - length of left string
 *     S2 - pointer to right string
 *     LEN2 - length of right string
 *
 * returns:
 *     1 if strings equal, 0 if not
 *
 * exceptions:
 *     none
 *
 * abstract:
 *     compares two character strings for equality
 *
 */

int
__eqstring (s1, len1, s2, len2)
     char *s1;
     int len1;
     char *s2;
     int len2;
{
  if (len1 != len2)
    return 0;

  return ! memcmp (s1, s2, len1);
}

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

#define MIN(a, b)  ((a) < (b) ? (a) : (b))


/*
 * function memmove
 *
 * parameters:
 *     S1 - pointer to destination string
 *     S2 - pointer to source string
 *     LEN - length of string
 *
 * returns:
 *     pointer to destination string
 *
 * exceptions:
 *     none
 *
 * abstract:
 *     copies a string safely, where the source and dest areas may overlap.
 *
 */

void *
memmove (s1, s2, n)
     void *s1;
     const void *s2;
     int n;
{
  char *sc1 = s1;
  const char *sc2 = s2;

  if (sc2 < sc1 && (sc1 < sc2 + n))
    for (sc1 += n, sc2 += n; 0 < n; --n)
      *--sc1 = *--sc2;
  else
#if 0
    for (; 0 < n; --n)
      *sc1++ = *sc2++;
#else
    memcpy (sc1, sc2, n);
#endif
  return s1;
}

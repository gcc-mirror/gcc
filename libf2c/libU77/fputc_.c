/* Copyright (C) 1996 Free Software Foundation, Inc.
This file is part of GNU Fortran libU77 library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdio.h>
#include "f2c.h"
#include "fio.h"

#ifdef KR_headers
integer G77_fputc_0 (lunit, c, Lc)
     integer *lunit;
     ftnlen Lc;			/* should be 1 */
     char *c;
#else
integer G77_fputc_0 (const integer *lunit, const char *c, const ftnlen Lc)
#endif
{
  int err;
  FILE *f = f__units[*lunit].ufd;

  if (*lunit>=MXUNIT || *lunit<0)
    return 101;			/* bad unit error */
  err = putc (c[0], f);
  if (err == EOF) {
    if (feof (f))
      return -1;
    else
      return ferror (f);
  }
  else
    return 0;
}

#ifdef KR_headers
integer G77_fput_0 (c, Lc)
     ftnlen Lc;			/* should be 1 */
     char *c;
#else
integer G77_fput_0 (const char *c, const ftnlen Lc)
#endif
{
  integer six = 6;

  return G77_fputc_0 (&six, c, Lc);
}

/* Copyright (C) 1995 Free Software Foundation, Inc.
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
#include <stdio.h>
#include "f2c.h"
#include "fio.h"

/* This flushes a single unit, c.f. libI77 version. */

extern integer G77_fnum_0 (integer *);

/* Subroutine */ int
G77_flush1_0 (const integer * lunit)
{
  if (*lunit >= MXUNIT || *lunit < 0)
    err (1, 101, "flush");
  /* f__units is a table of descriptions for the unit numbers (defined
     in io.h) with file descriptors rather than streams */
  if (f__units[*lunit].ufd != NULL && f__units[*lunit].uwrt)
    fflush (f__units[*lunit].ufd);
  return 0;
}

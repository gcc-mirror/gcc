/* Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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
#include <sys/types.h>
#if STDC_HEADERS
#  include <stdlib.h>
#endif
#if HAVE_UNISTD_H
#  include <unistd.h>		/* POSIX for ttyname */
#endif
#include <stdio.h>
#if HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <errno.h>		/* for ENOSYS */
#include "f2c.h"

extern integer G77_fnum_0 (integer * lunit);
extern void s_copy (register char *a, register char *b, ftnlen la, ftnlen lb);
/* Character */ void
G77_ttynam_0 (char *ret_val, ftnlen ret_val_len, integer * lunit)
{
#if defined (HAVE_TTYNAME)
  size_t i;
  char *p;

  p = ttyname (G77_fnum_0 (lunit));
  if (p != NULL)
    {
      i = strlen (p);
      s_copy (ret_val, p, ret_val_len, i);
    }
  else
    {
      s_copy (ret_val, " ", ret_val_len, 1);
    }
#else
  errno = ENOSYS;
  s_copy (ret_val, " ", ret_val_len, 1);
#endif
}

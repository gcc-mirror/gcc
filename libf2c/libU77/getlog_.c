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
#include <sys/types.h>
#if HAVE_STDLIB_H
#  include <stdlib.h>
#else
#  include <stdio.h>
#endif
#include <stdio.h>
#if HAVE_UNISTD_H
#  include <unistd.h>
#endif
#if HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <errno.h>		/* for ENOSYS */
#include "f2c.h"

/* getlogin not in svr1-3 */

/* SGI also has character*(*) function getlog() */

extern void s_copy (register char *a, register char *b, ftnlen la, ftnlen lb);
/* Subroutine */ int
G77_getlog_0 (char *str, const ftnlen Lstr)
{
  size_t i;
  char *p;
  int status;

#if defined (HAVE_GETLOGIN)
  p = getlogin ();
  if (p != NULL)
    {
      i = strlen (p);
      s_copy (str, p, Lstr, i);
    }
  else
    {
      s_copy (str, " ", Lstr, 1);
    }
  status = 0;
#else
  errno = ENOSYS;
  status = -1;
#endif
  return status;
}

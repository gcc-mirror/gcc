/* Copyright (C) 1995, 1997 Free Software Foundation, Inc.
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
#if HAVE_UNISTD_H
#  include <unistd.h>
#endif
#if HAVE_STDLIB_H
#  include <stdlib.h>
#else
#  include <stdio.h>
#endif

#include <errno.h>
#include <limits.h>
#include "f2c.h"

#ifndef R_OK			/* for SVR1-2 */
#  define R_OK 4
#endif
#ifndef W_OK
#  define W_OK 2
#endif
#ifndef X_OK
#  define X_OK 1
#endif
#ifndef F_OK
#  define F_OK 0
#endif

#ifdef KR_headers
void g_char ();

integer G77_access_0 (name, mode, Lname, Lmode)
     char *name, *mode;
     ftnlen Lname, Lmode;
#else
void g_char(const char *a, ftnlen alen, char *b);

integer G77_access_0 (const char *name, const char *mode, ftnlen Lname, ftnlen Lmode)
#endif
{
  char *buff;
  char *bp, *blast;
  int amode, i;

  buff = malloc (Lname+1);
  if (!buff) return -1;
  g_char (name, Lname, buff);
  amode = 0;
  for (i=0;i<Lmode;i++) {
    switch (mode[i]) {
    case 'r': amode |= R_OK; break;
    case 'w': amode |= W_OK; break;
    case 'x': amode |= X_OK; break;
    case ' ': amode |= F_OK; break; /* as per Sun, at least */
    default: return EINVAL;
    }
  }
  i = access (buff, amode);
  free (buff);
  return i;
}

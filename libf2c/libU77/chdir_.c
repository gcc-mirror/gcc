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
#include "f2c.h"


void g_char (const char *a, ftnlen alen, char *b);

integer
G77_chdir_0 (const char *name, const ftnlen Lname)
{
  char *buff;
  int i;

  buff = malloc (Lname + 1);
  if (!buff)
    return -1;
  g_char (name, Lname, buff);
  i = chdir (buff);
  free (buff);
  return i ? errno : 0;
}

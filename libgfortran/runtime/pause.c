/* Implementation of the STOP statement.
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfor).

Libgfor is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfor is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <string.h>
#include <stdio.h>

#include "libgfortran.h"

#define pause_numeric prefix(pause_numeric)
#define pause_string prefix(pause_string)

static void
do_pause (void)
{
  char buff[4];
  st_printf ("To resume execution, type go.  "
	     "Other input will terminate the job.\n");

  fgets(buff, 4, stdin);
  if (strncmp(buff, "go\n", 3) != 0)
    stop_numeric (-1);
  st_printf ("RESUMED\n");
}

/* A numeric or blank STOP statement.  */
void
pause_numeric (GFC_INTEGER_4 code)
{
  show_locus ();

  if (code == -1)
    st_printf ("PAUSE\n");
  else
    st_printf ("PAUSE %d\n", (int)code);

  do_pause ();
}


void
pause_string (char *string, GFC_INTEGER_4 len)
{
  show_locus ();

  st_printf ("PAUSE ");
  while (len--)
    st_printf ("%c", *(string++));
  st_printf ("\n");

  do_pause ();
}


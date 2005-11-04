/* Implementation of the STOP statement.
   Copyright 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include <string.h>

#include "libgfortran.h"

/* A numeric or blank STOP statement.  */
void
stop_numeric (GFC_INTEGER_4 code)
{
  show_locus ();

  if (code == -1)
    code = 0;
  else
    st_printf ("STOP %d\n", (int)code);

  sys_exit (code);
}
iexport(stop_numeric);


extern void stop_string (const char *string, GFC_INTEGER_4 len);
export_proto(stop_string);

void
stop_string (const char *string, GFC_INTEGER_4 len)
{
  show_locus ();

  st_printf ("STOP ");
  while (len--)
    st_printf ("%c", *(string++));
  st_printf ("\n");

  sys_exit (0);
}

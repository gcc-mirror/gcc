/* Implementation of the ISATTY and TTYNAM g77 intrinsics.
   Copyright (C) 2005 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

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
#include "libgfortran.h"
#include "../io/io.h"
#include <string.h>

/* LOGICAL FUNCTION ISATTY(UNIT)
   INTEGER, INTENT(IN) :: UNIT */

extern GFC_LOGICAL_4 isatty_l4 (int *);
export_proto(isatty_l4);

GFC_LOGICAL_4
isatty_l4 (int *unit)
{
  gfc_unit *u;

  u = find_unit (*unit);
  if (u != NULL)
    return (GFC_LOGICAL_4) stream_isatty (u->s);
  else
    return 0;
}


extern GFC_LOGICAL_8 isatty_l8 (int *);
export_proto(isatty_l8);

GFC_LOGICAL_8
isatty_l8 (int *unit)
{
  gfc_unit *u;

  u = find_unit (*unit);
  if (u != NULL)
    return (GFC_LOGICAL_8) stream_isatty (u->s);
  else
    return 0;
}


/* SUBROUTINE TTYNAM(UNIT,NAME)
   INTEGER,SCALAR,INTENT(IN) :: UNIT
   CHARACTER,SCALAR,INTENT(OUT) :: NAME */

extern void ttynam_sub (int *, char *, gfc_charlen_type);
export_proto(ttynam_sub);

void
ttynam_sub (int *unit, char * name, gfc_charlen_type name_len)
{
  gfc_unit *u;
  char * n;
  int i;

  memset (name, ' ', name_len);
  u = find_unit (*unit);
  if (u != NULL)
    {
      n = stream_ttyname (u->s);
      if (n != NULL)
	{
	  i = 0;
	  while (*n && i < name_len)
	    name[i++] = *(n++);
	}
    }
}

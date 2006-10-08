/* Generic implementation of the MOVE_ALLOC intrinsic
   Copyright (C) 2006 Free Software Foundation, Inc.
   Contributed by Paul Thomas

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

Ligbfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "libgfortran.h"

extern void move_alloc (gfc_array_char *, gfc_array_char *);
export_proto(move_alloc);

void
move_alloc (gfc_array_char * from, gfc_array_char * to)
{
  int i;

  internal_free (to->data);

  for (i = 0; i < GFC_DESCRIPTOR_RANK (from); i++)
    {
      to->dim[i].lbound = from->dim[i].lbound;
      to->dim[i].ubound = from->dim[i].ubound;
      to->dim[i].stride = from->dim[i].stride;
      from->dim[i].stride = 0;
      from->dim[i].ubound = from->dim[i].lbound;
    }

  to->offset = from->offset;
  to->dtype = from->dtype;
  to->data = from->data;
  from->data = NULL;
}

extern void move_alloc_c (gfc_array_char *, GFC_INTEGER_4,
			  gfc_array_char *, GFC_INTEGER_4);
export_proto(move_alloc_c);

void
move_alloc_c (gfc_array_char * from, GFC_INTEGER_4 from_length __attribute__((unused)),
	      gfc_array_char * to, GFC_INTEGER_4 to_length __attribute__((unused)))
{
  move_alloc (from, to);
}

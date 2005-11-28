/* Implementation of the FTELL intrinsic.
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

#include <string.h>

#include "../io/io.h"

extern size_t PREFIX(ftell) (int *);
export_proto_np(PREFIX(ftell));

size_t
PREFIX(ftell) (int * unit)
{
  gfc_unit * u = find_unit (*unit);
  size_t ret;
  if (u == NULL)
    return ((size_t) -1);
  ret = (size_t) stream_offset (u->s);
  unlock_unit (u);
  return ret;
}

#define FTELL_SUB(kind) \
  extern void ftell_i ## kind ## _sub (int *, GFC_INTEGER_ ## kind *); \
  export_proto(ftell_i ## kind ## _sub); \
  void \
  ftell_i ## kind ## _sub (int * unit, GFC_INTEGER_ ## kind * offset) \
  { \
    gfc_unit * u = find_unit (*unit); \
    if (u == NULL) \
      *offset = -1; \
    else \
      { \
	*offset = stream_offset (u->s); \
	unlock_unit (u); \
      } \
  }

FTELL_SUB(1)
FTELL_SUB(2)
FTELL_SUB(4)
FTELL_SUB(8)

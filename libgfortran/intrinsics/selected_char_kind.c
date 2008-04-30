/* Copyright 2008 Free Software Foundation, Inc.
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


#include "libgfortran.h"

#include <string.h>


extern GFC_INTEGER_4 selected_char_kind (gfc_charlen_type, char *);
export_proto(selected_char_kind);

GFC_INTEGER_4
selected_char_kind (gfc_charlen_type name_len, char *name)
{
  gfc_charlen_type len = fstrlen (name, name_len);

  if ((len == 5 && strncasecmp (name, "ascii", 5) == 0)
      || (len == 7 && strncasecmp (name, "default", 7) == 0))
    return 1;
  else
    return -1;
}

/* Copyright (C) 2008-2025 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#include "libgfortran.h"

#include <strings.h>


extern GFC_INTEGER_4 selected_char_kind (gfc_charlen_type, char *);
export_proto(selected_char_kind);

GFC_INTEGER_4
selected_char_kind (gfc_charlen_type name_len, char *name)
{
  gfc_charlen_type len = fstrlen (name, name_len);

  if ((len == 5 && strncasecmp (name, "ascii", 5) == 0)
      || (len == 7 && strncasecmp (name, "default", 7) == 0))
    return 1;
  else if (len == 9 && strncasecmp (name, "iso_10646", 9) == 0)
    return 4;
  else
    return -1;
}

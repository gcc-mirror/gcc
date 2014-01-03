/* Implementation of the PERROR intrinsic.
   Copyright (C) 2005-2014 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

This file is part of the GNU Fortran runtime library (libgfortran).

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

#include <errno.h>
#include <string.h>

/* SUBROUTINE PERROR(STRING)
   CHARACTER(len=*), INTENT(IN) :: STRING   */

extern void perror_sub (char *, gfc_charlen_type);
iexport_proto(perror_sub);

void
perror_sub (char *string, gfc_charlen_type string_len)
{
  char * str;

  /* Trim trailing spaces from paths.  */
  while (string_len > 0 && string[string_len - 1] == ' ')
    string_len--;

  /* Make a null terminated copy of the strings.  */
  str = gfc_alloca (string_len + 1);
  memcpy (str, string, string_len);
  str[string_len] = '\0';

  perror (str);
}
iexport(perror_sub);

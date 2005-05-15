/* Implementation of the PERROR intrinsic.
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
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "libgfortran.h"

#include <stdio.h>
#include <errno.h>

#include "../io/io.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

/* SUBROUTINE PERROR(STRING)
   CHARACTER(len=*), INTENT(IN) :: STRING   */

#ifdef HAVE_PERROR
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
#endif

/* Implementation of the CHDIR intrinsic.
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

#include <errno.h>

#include "../io/io.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* SUBROUTINE CHDIR(DIR, STATUS)
   CHARACTER(len=*), INTENT(IN) :: DIR
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS  */

#ifdef HAVE_CHDIR
extern void chdir_i4_sub (char *, GFC_INTEGER_4 *, gfc_charlen_type);
iexport_proto(chdir_i4_sub);

void
chdir_i4_sub (char *dir, GFC_INTEGER_4 *status, gfc_charlen_type dir_len)
{
  int val;
  char *str;

  /* Trim trailing spaces from paths.  */
  while (dir_len > 0 && dir[dir_len - 1] == ' ')
    dir_len--;

  /* Make a null terminated copy of the strings.  */
  str = gfc_alloca (dir_len + 1);
  memcpy (str, dir, dir_len);
  str[dir_len] = '\0';

  val = chdir (str);

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}
iexport(chdir_i4_sub);

extern void chdir_i8_sub (char *, GFC_INTEGER_8 *, gfc_charlen_type);
iexport_proto(chdir_i8_sub);

void
chdir_i8_sub (char *dir, GFC_INTEGER_8 *status, gfc_charlen_type dir_len)
{
  int val;
  char *str;

  /* Trim trailing spaces from paths.  */
  while (dir_len > 0 && dir[dir_len - 1] == ' ')
    dir_len--;

  /* Make a null terminated copy of the strings.  */
  str = gfc_alloca (dir_len + 1);
  memcpy (str, dir, dir_len);
  str[dir_len] = '\0';

  val = chdir (str);

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}
iexport(chdir_i8_sub);

extern GFC_INTEGER_4 chdir_i4 (char *, gfc_charlen_type);
export_proto(chdir_i4);

GFC_INTEGER_4
chdir_i4 (char *dir, gfc_charlen_type dir_len)
{
  GFC_INTEGER_4 val;
  chdir_i4_sub (dir, &val, dir_len);
  return val;
}

extern GFC_INTEGER_8 chdir_i8 (char *, gfc_charlen_type);
export_proto(chdir_i8);

GFC_INTEGER_8
chdir_i8 (char *dir, gfc_charlen_type dir_len)
{
  GFC_INTEGER_8 val;
  chdir_i8_sub (dir, &val, dir_len);
  return val;
}
#endif

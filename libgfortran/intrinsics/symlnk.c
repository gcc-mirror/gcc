/* Implementation of the SYMLNK intrinsic.
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

/* SUBROUTINE SYMLNK(PATH1, PATH2, STATUS)
   CHARACTER(len=*), INTENT(IN) :: PATH1, PATH2
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS  */

#ifdef HAVE_SYMLINK
extern void symlnk_i4_sub (char *, char *, GFC_INTEGER_4 *, gfc_charlen_type,
	                 gfc_charlen_type);
iexport_proto(symlnk_i4_sub);

void
symlnk_i4_sub (char *path1, char *path2, GFC_INTEGER_4 *status,
             gfc_charlen_type path1_len, gfc_charlen_type path2_len)
{
  int val;
  char *str1, *str2;

  /* Trim trailing spaces from paths.  */
  while (path1_len > 0 && path1[path1_len - 1] == ' ')
    path1_len--;
  while (path2_len > 0 && path2[path2_len - 1] == ' ')
    path2_len--;

  /* Make a null terminated copy of the strings.  */
  str1 = gfc_alloca (path1_len + 1);
  memcpy (str1, path1, path1_len);
  str1[path1_len] = '\0';

  str2 = gfc_alloca (path2_len + 1);
  memcpy (str2, path2, path2_len);
  str2[path2_len] = '\0';

  val = symlink (str1, str2);

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}
iexport(symlnk_i4_sub);

extern void symlnk_i8_sub (char *, char *, GFC_INTEGER_8 *, gfc_charlen_type,
	                 gfc_charlen_type);
iexport_proto(symlnk_i8_sub);

void
symlnk_i8_sub (char *path1, char *path2, GFC_INTEGER_8 *status,
             gfc_charlen_type path1_len, gfc_charlen_type path2_len)
{
  int val;
  char *str1, *str2;

  /* Trim trailing spaces from paths.  */
  while (path1_len > 0 && path1[path1_len - 1] == ' ')
    path1_len--;
  while (path2_len > 0 && path2[path2_len - 1] == ' ')
    path2_len--;

  /* Make a null terminated copy of the strings.  */
  str1 = gfc_alloca (path1_len + 1);
  memcpy (str1, path1, path1_len);
  str1[path1_len] = '\0';

  str2 = gfc_alloca (path2_len + 1);
  memcpy (str2, path2, path2_len);
  str2[path2_len] = '\0';

  val = symlink (str1, str2);

  if (status != NULL)
    *status = (val == 0) ? 0 : errno;
}
iexport(symlnk_i8_sub);

extern GFC_INTEGER_4 symlnk_i4 (char *, char *, gfc_charlen_type,
	                      gfc_charlen_type);
export_proto(symlnk_i4);

GFC_INTEGER_4
symlnk_i4 (char *path1, char *path2, gfc_charlen_type path1_len,
         gfc_charlen_type path2_len)
{
  GFC_INTEGER_4 val;
  symlnk_i4_sub (path1, path2, &val, path1_len, path2_len);
  return val;
}

extern GFC_INTEGER_8 symlnk_i8 (char *, char *, gfc_charlen_type,
	                      gfc_charlen_type);
export_proto(symlnk_i8);

GFC_INTEGER_8
symlnk_i8 (char *path1, char *path2, gfc_charlen_type path1_len,
	 gfc_charlen_type path2_len)
{
  GFC_INTEGER_8 val;
  symlnk_i8_sub (path1, path2, &val, path1_len, path2_len);
  return val;
}
#endif

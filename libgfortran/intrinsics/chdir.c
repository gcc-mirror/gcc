/* Implementation of the CHDIR intrinsic.
   Copyright (C) 2005-2017 Free Software Foundation, Inc.
   Contributed by François-Xavier Coudert <coudert@clipper.ens.fr>

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

#include <errno.h>

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
  char *str = fc_strdup (dir, dir_len);

  val = chdir (str);
  free (str);

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
  char *str = fc_strdup (dir, dir_len);

  val = chdir (str);
  free (str);

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

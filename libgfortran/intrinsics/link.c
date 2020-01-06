/* Implementation of the LINK intrinsic.
   Copyright (C) 2005-2020 Free Software Foundation, Inc.
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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* SUBROUTINE LINK(PATH1, PATH2, STATUS)
   CHARACTER(len=*), INTENT(IN) :: PATH1, PATH2
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS  */

#ifdef HAVE_LINK

static int
link_internal (char *path1, char *path2, gfc_charlen_type path1_len,
	       gfc_charlen_type path2_len)
{
  int val;
  char *str1, *str2;

  /* Make a null terminated copy of the strings.  */
  str1 = fc_strdup (path1, path1_len);
  str2 = fc_strdup (path2, path2_len);

  val = link (str1, str2);

  free (str1);
  free (str2);

  return ((val == 0) ? 0 : errno);
}


extern void link_i4_sub (char *, char *, GFC_INTEGER_4 *, gfc_charlen_type,
	                 gfc_charlen_type);
iexport_proto(link_i4_sub);

void
link_i4_sub (char *path1, char *path2, GFC_INTEGER_4 *status,
             gfc_charlen_type path1_len, gfc_charlen_type path2_len)
{
  int val = link_internal (path1, path2, path1_len, path2_len);

  if (status != NULL)
    *status = val;
}
iexport(link_i4_sub);

extern void link_i8_sub (char *, char *, GFC_INTEGER_8 *, gfc_charlen_type,
	                 gfc_charlen_type);
iexport_proto(link_i8_sub);

void
link_i8_sub (char *path1, char *path2, GFC_INTEGER_8 *status,
             gfc_charlen_type path1_len, gfc_charlen_type path2_len)
{
  int val = link_internal (path1, path2, path1_len, path2_len);

  if (status != NULL)
    *status = val;
}
iexport(link_i8_sub);

extern GFC_INTEGER_4 link_i4 (char *, char *, gfc_charlen_type,
	                      gfc_charlen_type);
export_proto(link_i4);

GFC_INTEGER_4
link_i4 (char *path1, char *path2, gfc_charlen_type path1_len,
         gfc_charlen_type path2_len)
{
  return link_internal (path1, path2, path1_len, path2_len);
}

extern GFC_INTEGER_8 link_i8 (char *, char *, gfc_charlen_type,
	                      gfc_charlen_type);
export_proto(link_i8);

GFC_INTEGER_8
link_i8 (char *path1, char *path2, gfc_charlen_type path1_len,
	 gfc_charlen_type path2_len)
{
  return link_internal (path1, path2, path1_len, path2_len);
}
#endif

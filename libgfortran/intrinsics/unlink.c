/* Implementation of the UNLINK intrinsic.
   Copyright (C) 2004-2025 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

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

/* SUBROUTINE UNLINK(NAME, STATUS)
   CHARACTER(LEN= ), INTENT(IN) :: NAME
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS)  */

extern void unlink_i4_sub (char *name, GFC_INTEGER_4 *status,
			   gfc_charlen_type name_len);
iexport_proto(unlink_i4_sub);

void
unlink_i4_sub (char *name, GFC_INTEGER_4 *status, gfc_charlen_type name_len)
{
  char *str;
  GFC_INTEGER_4 stat;

  /* Make a null terminated copy of the string.  */
  str = fc_strdup (name, name_len);

  stat = unlink (str);

  free (str);

  if (status != NULL)
    *status = (stat == 0) ? stat : errno;
}
iexport(unlink_i4_sub);

extern void unlink_i8_sub (char *name, GFC_INTEGER_8 *status,
			   gfc_charlen_type name_len);
export_proto(unlink_i8_sub);

void
unlink_i8_sub (char *name, GFC_INTEGER_8 *status, gfc_charlen_type name_len)
{
  GFC_INTEGER_4 status4;
  unlink_i4_sub (name, &status4, name_len);
  if (status)
    *status = status4;
}


/* INTEGER FUNCTION UNLINK(NAME)
   CHARACTER(LEN= ), INTENT(IN) :: NAME  */

extern GFC_INTEGER_4 PREFIX(unlink) (char *, gfc_charlen_type);
export_proto_np(PREFIX(unlink));

GFC_INTEGER_4
PREFIX(unlink) (char *name, gfc_charlen_type name_len)
{
  GFC_INTEGER_4 status;
  unlink_i4_sub (name, &status, name_len);
  return status;
}

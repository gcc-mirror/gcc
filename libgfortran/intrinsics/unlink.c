/* Implementation of the UNLINK intrinsic.
   Copyright (C) 2004 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with libgfor; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "libgfortran.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <errno.h>

/* SUBROUTINE UNLINK(NAME, STATUS)
   CHARACTER(LEN= ), INTENT(IN) :: NAME
   INTEGER, INTENT(OUT), OPTIONAL :: STATUS)  */

void
prefix(unlink_i4_sub) (char * name, GFC_INTEGER_4 * status,
		       gfc_charlen_type name_len)
{

  char *str, *s;
  GFC_INTEGER_4 stat;

  /* Trim trailing spaces from name.  */
  while (name_len > 0 && name[name_len - 1] == ' ')
    name_len--;

  /* Make a null terminated copy of the string.  */
  str = gfc_alloca (name_len + 1);
  memcpy (str, name, name_len);
  str[name_len] = '\0'; 

  stat = unlink (str);

  if (status != NULL) 
    *status = (stat == 0) ? stat : errno;
}


void
prefix(unlink_i8_sub) (char * name, GFC_INTEGER_8 * status,
		               gfc_charlen_type name_len)
{
  GFC_INTEGER_4 status4;
  prefix (unlink_i4_sub) (name, &status4, name_len);
  if (status)
    *status = status4;
}


/* INTEGER FUNCTION UNLINK(NAME)
   CHARACTER(LEN= ), INTENT(IN) :: NAME  */

GFC_INTEGER_4
prefix(unlink) (char * name, gfc_charlen_type name_len)
{
  GFC_INTEGER_4 status;
  prefix(unlink_i4_sub) (name, &status, name_len);
  return status;
}

/* Implementation of the GETCWD intrinsic.
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

void
prefix(getcwd_i4_sub) (char * cwd, GFC_INTEGER_4 * status,
		       gfc_charlen_type cwd_len)
{
  char str[cwd_len + 1], *s;
  GFC_INTEGER_4 stat;

  memset(cwd, ' ', (size_t) cwd_len);

  if (!getcwd (str, (size_t) cwd_len + 1))
    stat = errno;
  else
    {
      stat = 0;
      memcpy (cwd, str, strlen (str));
    }
  if (status != NULL) 
    *status = stat;
}

void
prefix(getcwd_i8_sub) (char * cwd, GFC_INTEGER_8 * status,
		               gfc_charlen_type cwd_len)
{
  GFC_INTEGER_4 status4;

  prefix (getcwd_i4_sub) (cwd, &status4, cwd_len);
  if (status)
    *status = status4;
}

GFC_INTEGER_4
prefix(getcwd) (char * cwd, gfc_charlen_type cwd_len)
{
  GFC_INTEGER_4 status;
  prefix(getcwd_i4_sub) (cwd, &status, cwd_len);
  return status;
}

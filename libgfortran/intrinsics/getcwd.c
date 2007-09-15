/* Implementation of the GETCWD intrinsic.
   Copyright (C) 2004, 2005, 2007 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

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

#include "libgfortran.h"

#include <string.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_GETCWD

extern void getcwd_i4_sub (char *, GFC_INTEGER_4 *, gfc_charlen_type);
iexport_proto(getcwd_i4_sub);

void
getcwd_i4_sub (char *cwd, GFC_INTEGER_4 *status, gfc_charlen_type cwd_len)
{
  char str[cwd_len + 1];
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
iexport(getcwd_i4_sub);

extern void getcwd_i8_sub (char *, GFC_INTEGER_8 *, gfc_charlen_type);
export_proto(getcwd_i8_sub);

void
getcwd_i8_sub (char *cwd, GFC_INTEGER_8 *status, gfc_charlen_type cwd_len)
{
  GFC_INTEGER_4 status4;
  getcwd_i4_sub (cwd, &status4, cwd_len);
  if (status)
    *status = status4;
}

extern GFC_INTEGER_4 PREFIX(getcwd) (char *, gfc_charlen_type);
export_proto_np(PREFIX(getcwd));

GFC_INTEGER_4
PREFIX(getcwd) (char *cwd, gfc_charlen_type cwd_len)
{
  GFC_INTEGER_4 status;
  getcwd_i4_sub (cwd, &status, cwd_len);
  return status;
}

#endif

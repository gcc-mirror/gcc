/* Implementation of the GETCWD intrinsic.
   Copyright (C) 2004-2021 Free Software Foundation, Inc.
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
  int err;

  if (getcwd (cwd, cwd_len))
    {
      size_t len = strlen (cwd);
      memset (cwd + len, ' ', cwd_len - len);
      err = 0;
    }
  else if (errno == ERANGE)
    {
      /* There is a possibility that the previous attempt failed due
	 to not enough space for the terminating null byte. Try again
	 with a buffer one char longer.  */
      char *buf = xmalloc (cwd_len + 1);
      if (getcwd (buf, cwd_len + 1))
	{
	  memcpy (cwd, buf, cwd_len);
	  err = 0;
	}
      else
	err = errno;
      free (buf);
    }
  else
    err = errno;
  if (err)
    memset (cwd, ' ', cwd_len);
  if (status != NULL)
    *status = err;
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

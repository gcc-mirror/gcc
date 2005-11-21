/* Copyright (C) 2002, 2003, 2005 Free Software Foundation, Inc.
   Contributed by Paul Brook

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

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

You should have received a copy of the GNU General Public License
along with libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include <string.h>

#include "libgfortran.h"
#include "../io/io.h"

/* Compare a C-style string with a fortran style string in a case-insensitive
   manner.  Used for decoding string options to various statements.  Returns
   zero if not equal, nonzero if equal.  */

static int
compare0 (const char *s1, int s1_len, const char *s2)
{
  int len;

  /* Strip trailing blanks from the Fortran string.  */
  len = fstrlen (s1, s1_len);
  return strncasecmp (s1, s2, len) == 0;
}


/* Given a fortran string, return its length exclusive of the trailing
   spaces.  */
int
fstrlen (const char *string, int len)
{
  for (len--; len >= 0; len--)
    if (string[len] != ' ')
      break;

  return len + 1;
}


void
fstrcpy (char *dest, int destlen, const char *src, int srclen)
{
  if (srclen >= destlen)
    {
      /* This will truncate if too long.  */
      memcpy (dest, src, destlen);
    }
  else
    {
      memcpy (dest, src, srclen);
      /* Pad with spaces.  */
      memset (&dest[srclen], ' ', destlen - srclen);
    }
}


void
cf_strcpy (char *dest, int dest_len, const char *src)
{
  int src_len;

  src_len = strlen (src);

  if (src_len >= dest_len)
    {
      /* This will truncate if too long.  */
      memcpy (dest, src, dest_len);
    }
  else
    {
      memcpy (dest, src, src_len);
      /* Pad with spaces.  */
      memset (&dest[src_len], ' ', dest_len - src_len);
    }
}


/* Given a fortran string and an array of st_option structures, search through
   the array to find a match.  If the option is not found, we generate an error
   if no default is provided.  */

int
find_option (st_parameter_common *cmp, const char *s1, int s1_len,
	     const st_option * opts, const char *error_message)
{
  for (; opts->name; opts++)
    if (compare0 (s1, s1_len, opts->name))
      return opts->value;

  generate_error (cmp, ERROR_BAD_OPTION, error_message);

  return -1;
}

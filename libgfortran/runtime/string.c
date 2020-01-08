/* Copyright (C) 2002-2020 Free Software Foundation, Inc.
   Contributed by Paul Brook

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

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
#include <strings.h>


/* Given a fortran string, return its length exclusive of the trailing
   spaces.  */

gfc_charlen_type
fstrlen (const char *string, gfc_charlen_type len)
{
  for (; len > 0; len--)
    if (string[len-1] != ' ')
      break;

  return len;
}


/* Copy a Fortran string (not null-terminated, hence length arguments
   for both source and destination strings. Returns the non-padded
   length of the destination.  */

gfc_charlen_type
fstrcpy (char *dest, gfc_charlen_type destlen, 
	 const char *src, gfc_charlen_type srclen)
{
  if (srclen >= destlen)
    {
      /* This will truncate if too long.  */
      memcpy (dest, src, destlen);
      return destlen;
    }
  else
    {
      memcpy (dest, src, srclen);
      /* Pad with spaces.  */
      memset (&dest[srclen], ' ', destlen - srclen);
      return srclen;
    }
}


/* Copy a null-terminated C string to a non-null-terminated Fortran
   string. Returns the non-padded length of the destination string.  */

gfc_charlen_type
cf_strcpy (char *dest, gfc_charlen_type dest_len, const char *src)
{
  size_t src_len;

  src_len = strlen (src);

  if (src_len >= (size_t) dest_len)
    {
      /* This will truncate if too long.  */
      memcpy (dest, src, dest_len);
      return dest_len;
    }
  else
    {
      memcpy (dest, src, src_len);
      /* Pad with spaces.  */
      memset (&dest[src_len], ' ', dest_len - src_len);
      return src_len;
    }
}


#ifndef HAVE_STRNLEN
static size_t
strnlen (const char *s, size_t maxlen)
{
  for (size_t ii = 0; ii < maxlen; ii++)
    {
      if (s[ii] == '\0')
	return ii;
    }
  return maxlen;
}
#endif


#ifndef HAVE_STRNDUP
static char *
strndup (const char *s, size_t n)
{
  size_t len = strnlen (s, n);
  char *p = malloc (len + 1);
  if (!p)
    return NULL;
  memcpy (p, s, len);
  p[len] = '\0';
  return p;
}
#endif


/* Duplicate a non-null-terminated Fortran string to a malloced
   null-terminated C string.  */

char *
fc_strdup (const char *src, gfc_charlen_type src_len)
{
  gfc_charlen_type n = fstrlen (src, src_len);
  char *p = strndup (src, n);
  if (!p)
    os_error ("Memory allocation failed in fc_strdup");
  return p;
}


/* Duplicate a non-null-terminated Fortran string to a malloced
   null-terminated C string, without getting rid of trailing
   blanks.  */

char *
fc_strdup_notrim (const char *src, gfc_charlen_type src_len)
{
  char *p = strndup (src, src_len);
  if (!p)
    os_error ("Memory allocation failed in fc_strdup");
  return p;
}


/* Given a fortran string and an array of st_option structures, search through
   the array to find a match.  If the option is not found, we generate an error
   if no default is provided.  */

int
find_option (st_parameter_common *cmp, const char *s1, gfc_charlen_type s1_len,
	     const st_option * opts, const char *error_message)
{
  /* Strip trailing blanks from the Fortran string.  */
  size_t len = (size_t) fstrlen (s1, s1_len);

  for (; opts->name; opts++)
    if (len == strlen(opts->name) && strncasecmp (s1, opts->name, len) == 0)
      return opts->value;

  generate_error (cmp, LIBERROR_BAD_OPTION, error_message);

  return -1;
}


/* gfc_itoa()-- Integer to decimal conversion.
   The itoa function is a widespread non-standard extension to
   standard C, often declared in <stdlib.h>.  Even though the itoa
   defined here is a static function we take care not to conflict with
   any prior non-static declaration.  Hence the 'gfc_' prefix, which
   is normally reserved for functions with external linkage.  Notably,
   in contrast to the *printf() family of functions, this ought to be
   async-signal-safe.  */

const char *
gfc_itoa (GFC_INTEGER_LARGEST n, char *buffer, size_t len)
{
  int negative;
  char *p;
  GFC_UINTEGER_LARGEST t;

  if (len < GFC_ITOA_BUF_SIZE)
    sys_abort ();

  if (n == 0)
    return "0";

  negative = 0;
  t = n;
  if (n < 0)
    {
      negative = 1;
      t = -n; /*must use unsigned to protect from overflow*/
    }

  p = buffer + GFC_ITOA_BUF_SIZE - 1;
  *p = '\0';

  while (t != 0)
    {
      *--p = '0' + (t % 10);
      t /= 10;
    }

  if (negative)
    *--p = '-';
  return p;
}

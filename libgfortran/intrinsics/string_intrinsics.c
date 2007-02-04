/* String intrinsics helper functions.
   Copyright 2002, 2005 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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


/* Unlike what the name of this file suggests, we don't actually
   implement the Fortran intrinsics here.  At least, not with the
   names they have in the standard.  The functions here provide all
   the support we need for the standard string intrinsics, and the
   compiler translates the actual intrinsics calls to calls to
   functions in this file.  */

#include <stdlib.h>
#include <string.h>

#include "libgfortran.h"


/* String functions.  */

extern void concat_string (GFC_INTEGER_4, char *,
			   GFC_INTEGER_4, const char *,
			   GFC_INTEGER_4, const char *);
export_proto(concat_string);

extern GFC_INTEGER_4 string_len_trim (GFC_INTEGER_4, const char *);
export_proto(string_len_trim);

extern void adjustl (char *, GFC_INTEGER_4, const char *);
export_proto(adjustl);

extern void adjustr (char *, GFC_INTEGER_4, const char *);
export_proto(adjustr);

extern GFC_INTEGER_4 string_index (GFC_INTEGER_4, const char *, GFC_INTEGER_4,
				   const char *, GFC_LOGICAL_4);
export_proto(string_index);

extern GFC_INTEGER_4 string_scan (GFC_INTEGER_4, const char *, GFC_INTEGER_4,
				  const char *, GFC_LOGICAL_4);
export_proto(string_scan);

extern GFC_INTEGER_4 string_verify (GFC_INTEGER_4, const char *, GFC_INTEGER_4,
				    const char *, GFC_LOGICAL_4);
export_proto(string_verify);

extern void string_trim (GFC_INTEGER_4 *, void **, GFC_INTEGER_4, const char *);
export_proto(string_trim);

extern void string_repeat (char *, GFC_INTEGER_4, const char *, GFC_INTEGER_4);
export_proto(string_repeat);

/* Strings of unequal length are extended with pad characters.  */

GFC_INTEGER_4
compare_string (GFC_INTEGER_4 len1, const char * s1,
		GFC_INTEGER_4 len2, const char * s2)
{
  int res;
  const unsigned char *s;
  int len;

  res = memcmp (s1, s2, (len1 < len2) ? len1 : len2);
  if (res != 0)
    return res;

  if (len1 == len2)
    return 0;

  if (len1 < len2)
    {
      len = len2 - len1;
      s = (unsigned char *) &s2[len1];
      res = -1;
    }
  else
    {
      len = len1 - len2;
      s = (unsigned char *) &s1[len2];
      res = 1;
    }

  while (len--)
    {
      if (*s != ' ')
        {
          if (*s > ' ')
            return res;
          else
            return -res;
        }
      s++;
    }

  return 0;
}
iexport(compare_string);


/* The destination and source should not overlap.  */

void
concat_string (GFC_INTEGER_4 destlen, char * dest,
	       GFC_INTEGER_4 len1, const char * s1,
	       GFC_INTEGER_4 len2, const char * s2)
{
  if (len1 >= destlen)
    {
      memcpy (dest, s1, destlen);
      return;
    }
  memcpy (dest, s1, len1);
  dest += len1;
  destlen -= len1;

  if (len2 >= destlen)
    {
      memcpy (dest, s2, destlen);
      return;
    }

  memcpy (dest, s2, len2);
  memset (&dest[len2], ' ', destlen - len2);
}


/* Return string with all trailing blanks removed.  */

void
string_trim (GFC_INTEGER_4 * len, void ** dest, GFC_INTEGER_4 slen,
	     const char * src)
{
  int i;

  /* Determine length of result string.  */
  for (i = slen - 1; i >= 0; i--)
    {
      if (src[i] != ' ')
        break;
    }
  *len = i + 1;

  if (*len > 0)
    {
      /* Allocate space for result string.  */
      *dest = internal_malloc_size (*len);

      /* copy string if necessary.  */
      memmove (*dest, src, *len);
    }
}


/* The length of a string not including trailing blanks.  */

GFC_INTEGER_4
string_len_trim (GFC_INTEGER_4 len, const char * s)
{
  int i;

  for (i = len - 1; i >= 0; i--)
    {
      if (s[i] != ' ')
        break;
    }
  return i + 1;
}


/* Find a substring within a string.  */

GFC_INTEGER_4
string_index (GFC_INTEGER_4 slen, const char * str, GFC_INTEGER_4 sslen,
	      const char * sstr, GFC_LOGICAL_4 back)
{
  int start;
  int last;
  int i;
  int delta;

  if (sslen == 0)
    return 1;

  if (sslen > slen)
    return 0;

  if (!back)
    {
      last = slen + 1 - sslen;
      start = 0;
      delta = 1;
    }
  else
    {
      last = -1;
      start = slen - sslen;
      delta = -1;
    }
  i = 0;
  for (; start != last; start+= delta)
    {
      for (i = 0; i < sslen; i++)
        {
          if (str[start + i] != sstr[i])
            break;
        }
      if (i == sslen)
        return (start + 1);
    }
  return 0;
}


/* Remove leading blanks from a string, padding at end.  The src and dest
   should not overlap.  */

void
adjustl (char *dest, GFC_INTEGER_4 len, const char *src)
{
  int i;

  i = 0;
  while (i<len && src[i] == ' ')
    i++;

  if (i < len)
    memcpy (dest, &src[i], len - i);
  if (i > 0)
    memset (&dest[len - i], ' ', i);
}


/* Remove trailing blanks from a string.  */

void
adjustr (char *dest, GFC_INTEGER_4 len, const char *src)
{
  int i;

  i = len;
  while (i > 0 && src[i - 1] == ' ')
    i--;

  if (i < len)
    memset (dest, ' ', len - i);
  memcpy (dest + (len - i), src, i );
}


/* Scan a string for any one of the characters in a set of characters.  */

GFC_INTEGER_4
string_scan (GFC_INTEGER_4 slen, const char * str, GFC_INTEGER_4 setlen,
             const char * set, GFC_LOGICAL_4 back)
{
  int i, j;

  if (slen == 0 || setlen == 0)
    return 0;

  if (back)
    {
      for (i = slen - 1; i >= 0; i--)
	{
	  for (j = 0; j < setlen; j++)
	    {
	      if (str[i] == set[j])
		return (i + 1);
	    }
	}
    }
  else
    {
      for (i = 0; i < slen; i++)
	{
	  for (j = 0; j < setlen; j++)
	    {
	      if (str[i] == set[j])
		return (i + 1);
	    }
	}
    }

  return 0;
}


/* Verify that a set of characters contains all the characters in a
   string by identifying the position of the first character in a
   characters that does not appear in a given set of characters.  */

GFC_INTEGER_4
string_verify (GFC_INTEGER_4 slen, const char * str, GFC_INTEGER_4 setlen,
               const char * set, GFC_LOGICAL_4 back)
{
  int start;
  int last;
  int i;
  int delta;

  if (slen == 0)
    return 0;

  if (back)
    {
      last = -1;
      start = slen - 1;
      delta = -1;
    }
  else
    {
      last = slen;
      start = 0;
      delta = 1;
    }
  for (; start != last; start += delta)
    {
      for (i = 0; i < setlen; i++)
        {
          if (str[start] == set[i])
            break;
        }
      if (i == setlen)
        return (start + 1);
    }

  return 0;
}


/* Concatenate several copies of a string.  */

void
string_repeat (char * dest, GFC_INTEGER_4 slen, 
               const char * src, GFC_INTEGER_4 ncopies)
{
  int i;

  /* We don't need to check that ncopies is non-negative here, because
     the front-end already generates code for that check.  */
  for (i = 0; i < ncopies; i++) 
    {
      memmove (dest + (i * slen), src, slen);
    }
}

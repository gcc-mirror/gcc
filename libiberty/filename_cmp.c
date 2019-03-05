/* File name comparison routine.

   Copyright (C) 2007-2019 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "filenames.h"
#include "safe-ctype.h"
#include "libiberty.h"

/*

@deftypefn Extension int filename_cmp (const char *@var{s1}, const char *@var{s2})

Return zero if the two file names @var{s1} and @var{s2} are equivalent.
If not equivalent, the returned value is similar to what @code{strcmp}
would return.  In other words, it returns a negative value if @var{s1}
is less than @var{s2}, or a positive value if @var{s2} is greater than
@var{s2}.

This function does not normalize file names.  As a result, this function
will treat filenames that are spelled differently as different even in
the case when the two filenames point to the same underlying file.
However, it does handle the fact that on DOS-like file systems, forward
and backward slashes are equal.

@end deftypefn

*/

int
filename_cmp (const char *s1, const char *s2)
{
#if !defined(HAVE_DOS_BASED_FILE_SYSTEM) \
    && !defined(HAVE_CASE_INSENSITIVE_FILE_SYSTEM)
  return strcmp(s1, s2);
#else
  for (;;)
    {
      int c1 = *s1;
      int c2 = *s2;

#if defined (HAVE_CASE_INSENSITIVE_FILE_SYSTEM)
      c1 = TOLOWER (c1);
      c2 = TOLOWER (c2);
#endif

#if defined (HAVE_DOS_BASED_FILE_SYSTEM)
      /* On DOS-based file systems, the '/' and the '\' are equivalent.  */
      if (c1 == '/')
        c1 = '\\';
      if (c2 == '/')
        c2 = '\\';
#endif

      if (c1 != c2)
        return (c1 - c2);

      if (c1 == '\0')
        return 0;

      s1++;
      s2++;
    }
#endif
}

/*

@deftypefn Extension int filename_ncmp (const char *@var{s1}, const char *@var{s2}, size_t @var{n})

Return zero if the two file names @var{s1} and @var{s2} are equivalent
in range @var{n}.
If not equivalent, the returned value is similar to what @code{strncmp}
would return.  In other words, it returns a negative value if @var{s1}
is less than @var{s2}, or a positive value if @var{s2} is greater than
@var{s2}.

This function does not normalize file names.  As a result, this function
will treat filenames that are spelled differently as different even in
the case when the two filenames point to the same underlying file.
However, it does handle the fact that on DOS-like file systems, forward
and backward slashes are equal.

@end deftypefn

*/

int
filename_ncmp (const char *s1, const char *s2, size_t n)
{
#if !defined(HAVE_DOS_BASED_FILE_SYSTEM) \
    && !defined(HAVE_CASE_INSENSITIVE_FILE_SYSTEM)
  return strncmp(s1, s2, n);
#else
  if (!n)
    return 0;
  for (; n > 0; --n)
  {
      int c1 = *s1;
      int c2 = *s2;

#if defined (HAVE_CASE_INSENSITIVE_FILE_SYSTEM)
      c1 = TOLOWER (c1);
      c2 = TOLOWER (c2);
#endif

#if defined (HAVE_DOS_BASED_FILE_SYSTEM)
      /* On DOS-based file systems, the '/' and the '\' are equivalent.  */
      if (c1 == '/')
        c1 = '\\';
      if (c2 == '/')
        c2 = '\\';
#endif

      if (c1 == '\0' || c1 != c2)
        return (c1 - c2);

      s1++;
      s2++;
  }
  return 0;
#endif
}

/*

@deftypefn Extension hashval_t filename_hash (const void *@var{s})

Return the hash value for file name @var{s} that will be compared
using filename_cmp.
This function is for use with hashtab.c hash tables.

@end deftypefn

*/

hashval_t
filename_hash (const void *s)
{
  /* The cast is for -Wc++-compat.  */
  const unsigned char *str = (const unsigned char *) s;
  hashval_t r = 0;
  unsigned char c;

  while ((c = *str++) != 0)
    {
      if (c == '\\')
	c = '/';
      c = TOLOWER (c);
      r = r * 67 + c - 113;
    }

  return r;
}

/*

@deftypefn Extension int filename_eq (const void *@var{s1}, const void *@var{s2})

Return non-zero if file names @var{s1} and @var{s2} are equivalent.
This function is for use with hashtab.c hash tables.

@end deftypefn

*/

int
filename_eq (const void *s1, const void *s2)
{
  /* The casts are for -Wc++-compat.  */
  return filename_cmp ((const char *) s1, (const char *) s2) == 0;
}

/*

@deftypefn Extension int canonical_filename_eq (const char *@var{a}, const char *@var{b})

Return non-zero if file names @var{a} and @var{b} are equivalent.
This function compares the canonical versions of the filenames as returned by
@code{lrealpath()}, so that so that different file names pointing to the same
underlying file are treated as being identical.

@end deftypefn

*/

int
canonical_filename_eq (const char * a, const char * b)
{
  char * ca = lrealpath(a);
  char * cb = lrealpath(b);
  int res = filename_eq (ca, cb);
  free (ca);
  free (cb);
  return res;
}

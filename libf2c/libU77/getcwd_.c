/* Copyright (C) 1995, 1996 Free Software Foundation, Inc.
This file is part of GNU Fortran libU77 library.

This library is free software; you can redistribute it and/or modify it
under the terms of the GNU Library General Public License as published
by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with GNU Fortran; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <errno.h>
#if HAVE_STRING_H
#  include <string.h>
#else
#  include <strings.h>
#endif
#include <stdio.h>		/* for NULL */
#include <errno.h>		/* for ENOSYS */
#include "f2c.h"

#if HAVE_GETCWD

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#else
extern char *getcwd ();
#endif

extern void s_copy (register char *a, register char *b, ftnlen la, ftnlen lb);
integer
G77_getcwd_0 (char *str, const ftnlen Lstr)
{
  int i;
  char *ret;

  ret = getcwd (str, Lstr);
  if (ret == NULL)
    return errno;
  for (i = strlen (str); i < Lstr; i++)
    str[i] = ' ';
  return 0;
}

#elif HAVE_GETWD		/* HAVE_GETCWD */

/* getwd usage taken from SunOS4 man */

#  include <sys/param.h>
extern char *getwd ();
extern void s_copy (register char *a, register char *b, ftnlen la, ftnlen lb);
integer
G77_getcwd_0 (char *str, const ftnlen Lstr)
{
  char pathname[MAXPATHLEN];
  size_t l;

  if (getwd (pathname) == NULL)
    {
      return errno;
    }
  else
    {
      s_copy (str, pathname, Lstr, strlen (str));
      return 0;
    }
}

#else /* !HAVE_GETWD && !HAVE_GETCWD */

extern void s_copy (register char *a, register char *b, ftnlen la, ftnlen lb);
integer
G77_getcwd_0 (char *str, const ftnlen Lstr)
{
  return errno = ENOSYS;
}

#endif

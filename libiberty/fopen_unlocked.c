/* Implement fopen_unlocked and related functions.
   Copyright (C) 2005 Free Software Foundation, Inc.
   Written by Kaveh R. Ghazi <ghazi@caip.rutgers.edu>.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/*

@deftypefn Extension FILE * fopen_unlocked (const char *@var{path}, const char * @var{mode})

Opens and returns a @code{FILE} pointer via @code{fopen}.  If the
operating system supports it, ensure that the stream is setup to avoid
any multi-threaded locking.  Otherwise return the @code{FILE} pointer
unchanged.

@end deftypefn

@deftypefn Extension FILE * fdopen_unlocked (int @var{fildes}, const char * @var{mode})

Opens and returns a @code{FILE} pointer via @code{fdopen}.  If the
operating system supports it, ensure that the stream is setup to avoid
any multi-threaded locking.  Otherwise return the @code{FILE} pointer
unchanged.

@end deftypefn

@deftypefn Extension FILE * freopen_unlocked (const char * @var{path}, const char * @var{mode}, FILE * @var{stream})

Opens and returns a @code{FILE} pointer via @code{freopen}.  If the
operating system supports it, ensure that the stream is setup to avoid
any multi-threaded locking.  Otherwise return the @code{FILE} pointer
unchanged.

@end deftypefn

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdio.h>
#ifdef HAVE_STDIO_EXT_H
#include <stdio_ext.h>
#endif

#include "libiberty.h"

FILE *
fopen_unlocked (const char *path, const char *mode)		
{
  FILE *const fp = fopen (path, mode);
#if defined(HAVE___FSETLOCKING) && defined(FSETLOCKING_BYCALLER)
  if (fp)
    __fsetlocking (fp, FSETLOCKING_BYCALLER);
#endif
  return fp;
}

FILE *
fdopen_unlocked (int fildes, const char *mode)
{
  FILE *const fp = fdopen (fildes, mode);
#if defined(HAVE___FSETLOCKING) && defined(FSETLOCKING_BYCALLER)
  if (fp)
    __fsetlocking (fp, FSETLOCKING_BYCALLER);
#endif
  return fp;
}

FILE *
freopen_unlocked (const char *path, const char *mode, FILE *stream)
{
  FILE *const fp = freopen (path, mode, stream);
#if defined(HAVE___FSETLOCKING) && defined(FSETLOCKING_BYCALLER)
  if (fp)
    __fsetlocking (fp, FSETLOCKING_BYCALLER);
#endif
  return fp;
}

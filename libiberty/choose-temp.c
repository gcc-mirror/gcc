/* Utility to pick a temporary filename prefix.
   Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.

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
License along with libiberty; see the file COPYING.LIB.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This file exports two functions: choose_temp_base and make_temp_file.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>	/* May get P_tmpdir.  */
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>   /* May get R_OK, etc. on some systems.  */
#endif

#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#endif

#include "libiberty.h"
extern int mkstemps ();

#ifndef IN_GCC
#if defined (__MSDOS__) || (defined (_WIN32) && ! defined (__CYGWIN__) && ! defined (_UWIN))
#define DIR_SEPARATOR '\\'
#endif
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

/* On MSDOS, write temp files in current dir
   because there's no place else we can expect to use.  */
/* ??? Although the current directory is tried as a last resort,
   this is left in so that on MSDOS it is preferred to /tmp on the
   off chance that someone requires this, since that was the previous
   behaviour.  */
#ifdef __MSDOS__
#ifndef P_tmpdir
#define P_tmpdir "."
#endif
#endif

/* Name of temporary file.
   mktemp requires 6 trailing X's.  */
#define TEMP_FILE "ccXXXXXX"

/* Subroutine of choose_temp_base.
   If BASE is non-NULL, return it.
   Otherwise it checks if DIR is a usable directory.
   If success, DIR is returned.
   Otherwise NULL is returned.  */

static char *
try (dir, base)
     char *dir, *base;
{
  if (base != 0)
    return base;
  if (dir != 0
      && access (dir, R_OK | W_OK | X_OK) == 0)
    return dir;
  return 0;
}

/* Return a prefix for temporary file names or NULL if unable to find one.
   The current directory is chosen if all else fails so the program is
   exited if a temporary directory can't be found (mktemp fails).
   The buffer for the result is obtained with xmalloc. 

   This function is provided for backwards compatability only.  It use
   is not recommended.  */

char *
choose_temp_base ()
{
  char *base = 0;
  char *temp_filename;
  int len;
  static char tmp[] = { DIR_SEPARATOR, 't', 'm', 'p', 0 };
  static char usrtmp[] = { DIR_SEPARATOR, 'u', 's', 'r', DIR_SEPARATOR, 't', 'm', 'p', 0 };

  base = try (getenv ("TMPDIR"), base);
  base = try (getenv ("TMP"), base);
  base = try (getenv ("TEMP"), base);

#ifdef P_tmpdir
  base = try (P_tmpdir, base);
#endif

  /* Try /usr/tmp, then /tmp.  */
  base = try (usrtmp, base);
  base = try (tmp, base);
 
  /* If all else fails, use the current directory!  */
  if (base == 0)
    base = ".";

  len = strlen (base);
  temp_filename = xmalloc (len + 1 /*DIR_SEPARATOR*/
			   + strlen (TEMP_FILE) + 1);
  strcpy (temp_filename, base);

  if (len != 0
      && temp_filename[len-1] != '/'
      && temp_filename[len-1] != DIR_SEPARATOR)
    temp_filename[len++] = DIR_SEPARATOR;
  strcpy (temp_filename + len, TEMP_FILE);

  mktemp (temp_filename);
  if (strlen (temp_filename) == 0)
    abort ();
  return temp_filename;
}
/* Return a temporary file name (as a string) or NULL if unable to create
   one.  */

char *
make_temp_file (suffix)
     const char *suffix;
{
  char *base = 0;
  char *temp_filename;
  int base_len, suffix_len;
  int fd;
  static char tmp[] = { DIR_SEPARATOR, 't', 'm', 'p', 0 };
  static char usrtmp[] = { DIR_SEPARATOR, 'u', 's', 'r', DIR_SEPARATOR, 't', 'm', 'p', 0 };

  base = try (getenv ("TMPDIR"), base);
  base = try (getenv ("TMP"), base);
  base = try (getenv ("TEMP"), base);

#ifdef P_tmpdir
  base = try (P_tmpdir, base);
#endif

  /* Try /usr/tmp, then /tmp.  */
  base = try (usrtmp, base);
  base = try (tmp, base);
 
  /* If all else fails, use the current directory!  */
  if (base == 0)
    base = ".";

  base_len = strlen (base);

  if (suffix)
    suffix_len = strlen (suffix);
  else
    suffix_len = 0;

  temp_filename = xmalloc (base_len + 1 /*DIR_SEPARATOR*/
			   + strlen (TEMP_FILE)
			   + suffix_len + 1);
  strcpy (temp_filename, base);

  if (base_len != 0
      && temp_filename[base_len-1] != '/'
      && temp_filename[base_len-1] != DIR_SEPARATOR)
    temp_filename[base_len++] = DIR_SEPARATOR;
  strcpy (temp_filename + base_len, TEMP_FILE);

  if (suffix)
    strcat (temp_filename, suffix);

  fd = mkstemps (temp_filename, suffix_len);
  /* If mkstemps failed, then something bad is happening.  Maybe we should
     issue a message about a possible security attack in progress?  */
  if (fd == -1)
    abort ();
  /* Similarly if we can not close the file.  */
  if (close (fd))
    abort ();
  return temp_filename;
}

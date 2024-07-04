/* Utility to pick a temporary filename prefix.
   Copyright (C) 1996-2024 Free Software Foundation, Inc.

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
write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>	/* May get P_tmpdir.  */
#include <sys/types.h>
#include <errno.h>
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
#if defined(_WIN32) && !defined(__CYGWIN__)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif


#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#endif

#include "libiberty.h"
extern int mkstemps (char *, int);

/* '/' works just fine on MS-DOS based systems.  */
#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

/* Name of temporary file.
   mktemp requires 6 trailing X's.  */
#define TEMP_FILE "XXXXXX"
#define TEMP_FILE_LEN (sizeof(TEMP_FILE) - 1)

#if !defined(_WIN32) || defined(__CYGWIN__)

/* Subroutine of choose_tmpdir.
   If BASE is non-NULL, return it.
   Otherwise it checks if DIR is a usable directory.
   If success, DIR is returned.
   Otherwise NULL is returned.  */

static inline const char *try_dir (const char *, const char *);

static inline const char *
try_dir (const char *dir, const char *base)
{
  if (base != 0)
    return base;
  if (dir != 0
      && access (dir, R_OK | W_OK | X_OK) == 0)
    {
      /* Check to make sure dir is actually a directory. */
#ifdef S_ISDIR
      struct stat s;
      if (stat (dir, &s))
	return NULL;
      if (!S_ISDIR (s.st_mode))
	return NULL;
#endif
      return dir;
    }
  return 0;
}

static const char tmp[] = { DIR_SEPARATOR, 't', 'm', 'p', 0 };
static const char vartmp[] =
{ DIR_SEPARATOR, 'v', 'a', 'r', DIR_SEPARATOR, 't', 'm', 'p', 0 };

#endif

static char *memoized_tmpdir;

/*

@deftypefn Replacement const char* choose_tmpdir ()

Returns a pointer to a directory path suitable for creating temporary
files in.

@end deftypefn

*/

const char *
choose_tmpdir (void)
{
  if (!memoized_tmpdir)
    {
#if !defined(_WIN32) || defined(__CYGWIN__)
      const char *base = 0;
      char *tmpdir;
      unsigned int len;
      
#ifdef VMS
      /* Try VMS standard temp logical.  */
      base = try_dir ("/sys$scratch", base);
#else
      base = try_dir (getenv ("TMPDIR"), base);
      base = try_dir (getenv ("TMP"), base);
      base = try_dir (getenv ("TEMP"), base);
#endif
      
#ifdef P_tmpdir
      /* We really want a directory name here as if concatenated with say \dir
	 we do not end up with a double \\ which defines an UNC path.  */
      if (strcmp (P_tmpdir, "\\") == 0)
	base = try_dir ("\\.", base);
      else
	base = try_dir (P_tmpdir, base);
#endif

      /* Try /var/tmp, then /tmp.  */
      base = try_dir (vartmp, base);
      base = try_dir (tmp, base);
      
      /* If all else fails, use the current directory!  */
      if (base == 0)
	base = ".";
      /* Append DIR_SEPARATOR to the directory we've chosen
	 and return it.  */
      len = strlen (base);
      tmpdir = XNEWVEC (char, len + 2);
      strcpy (tmpdir, base);
      tmpdir[len] = DIR_SEPARATOR;
      tmpdir[len+1] = '\0';
      memoized_tmpdir = tmpdir;
#else /* defined(_WIN32) && !defined(__CYGWIN__) */
      DWORD len;

      /* Figure out how much space we need.  */
      len = GetTempPath(0, NULL);
      if (len)
	{
	  memoized_tmpdir = XNEWVEC (char, len);
	  if (!GetTempPath(len, memoized_tmpdir))
	    {
	      XDELETEVEC (memoized_tmpdir);
	      memoized_tmpdir = NULL;
	    }
	}
      if (!memoized_tmpdir)
	/* If all else fails, use the current directory.  */
	memoized_tmpdir = xstrdup (".\\");
#endif /* defined(_WIN32) && !defined(__CYGWIN__) */
    }

  return memoized_tmpdir;
}

/*

@deftypefn Replacement char* make_temp_file (const char *@var{suffix})

Return a temporary file name (as a string) or @code{NULL} if unable to
create one.  @var{suffix} is a suffix to append to the file name.  The
string is @code{malloc}ed, and the temporary file has been created.

@end deftypefn

*/

char *
make_temp_file_with_prefix (const char *prefix, const char *suffix)
{
  const char *base = choose_tmpdir ();
  char *temp_filename;
  int base_len, suffix_len, prefix_len;
  int fd;

  if (prefix == 0)
    prefix = "cc";

  if (suffix == 0)
    suffix = "";

  base_len = strlen (base);
  prefix_len = strlen (prefix);
  suffix_len = strlen (suffix);

  temp_filename = XNEWVEC (char, base_len
			   + TEMP_FILE_LEN
			   + suffix_len
			   + prefix_len + 1);
  strcpy (temp_filename, base);
  strcpy (temp_filename + base_len, prefix);
  strcpy (temp_filename + base_len + prefix_len, TEMP_FILE);
  strcpy (temp_filename + base_len + prefix_len + TEMP_FILE_LEN, suffix);

  fd = mkstemps (temp_filename, suffix_len);
  /* Mkstemps failed.  It may be EPERM, ENOSPC etc.  */
  if (fd == -1)
    {
      fprintf (stderr, "Cannot create temporary file in %s: %s\n",
	       base, strerror (errno));
      abort ();
    }
  /* We abort on failed close out of sheer paranoia.  */
  if (close (fd))
    abort ();
  return temp_filename;
}

char *
make_temp_file (const char *suffix)
{
  return make_temp_file_with_prefix (NULL, suffix);
}

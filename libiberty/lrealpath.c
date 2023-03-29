/* Libiberty realpath.  Like realpath, but more consistent behavior.
   Based on gdb_realpath from GDB.

   Copyright (C) 2003-2023 Free Software Foundation, Inc.

   This file is part of the libiberty library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/*

@deftypefn Replacement {const char*} lrealpath (const char *@var{name})

Given a pointer to a string containing a pathname, returns a canonical
version of the filename.  Symlinks will be resolved, and ``.'' and ``..''
components will be simplified.  The returned value will be allocated using
@code{malloc}, or @code{NULL} will be returned on a memory allocation error.

@end deftypefn

*/

#include "config.h"
#include "ansidecl.h"
#include "libiberty.h"

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

/* On GNU libc systems the declaration is only visible with _GNU_SOURCE.  */
#if defined(HAVE_CANONICALIZE_FILE_NAME) \
    && defined(NEED_DECLARATION_CANONICALIZE_FILE_NAME)
extern char *canonicalize_file_name (const char *);
#endif

#if defined(HAVE_REALPATH)
# if defined (PATH_MAX)
#  define REALPATH_LIMIT PATH_MAX
# else
#  if defined (MAXPATHLEN)
#   define REALPATH_LIMIT MAXPATHLEN
#  endif
# endif
#else
  /* cygwin has realpath, so it won't get here.  */ 
# if defined (_WIN32)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h> /* for GetFullPathName/GetFinalPathNameByHandle/
                          CreateFile/CloseHandle */
#  define WIN32_REPLACE_SLASHES(_ptr, _len) \
     for (unsigned i = 0; i != (_len); ++i) \
       if ((_ptr)[i] == '\\') (_ptr)[i] = '/';

#  define WIN32_UNC_PREFIX "//?/UNC/"
#  define WIN32_UNC_PREFIX_LEN (sizeof(WIN32_UNC_PREFIX)-1)
#  define WIN32_IS_UNC_PREFIX(ptr) \
  (0 == memcmp(ptr, WIN32_UNC_PREFIX, WIN32_UNC_PREFIX_LEN))

#  define WIN32_NON_UNC_PREFIX "//?/"
#  define WIN32_NON_UNC_PREFIX_LEN (sizeof(WIN32_NON_UNC_PREFIX)-1)
#  define WIN32_IS_NON_UNC_PREFIX(ptr) \
  (0 == memcmp(ptr, WIN32_NON_UNC_PREFIX, WIN32_NON_UNC_PREFIX_LEN))

/* Get full path name without symlinks resolution.
   It also converts all forward slashes to back slashes.
*/
char* get_full_path_name(const char *filename) {
  DWORD len;
  char *buf, *ptr, *res;

  /* determining the required buffer size.
     from the man: `If the lpBuffer buffer is too small to contain
     the path, the return value is the size, in TCHARs, of the buffer
     that is required to hold the path _and_the_terminating_null_character_`
  */
  len = GetFullPathName(filename, 0, NULL, NULL);

  if ( len == 0 )
    return strdup(filename);

  buf = (char *)malloc(len);

  /* no point to check the result again */
  len = GetFullPathName(filename, len, buf, NULL);
  buf[len] = 0;

  /* replace slashes */
  WIN32_REPLACE_SLASHES(buf, len);

  /* calculate offset based on prefix type */
  len = WIN32_IS_UNC_PREFIX(buf)
    ? (WIN32_UNC_PREFIX_LEN - 2)
    : WIN32_IS_NON_UNC_PREFIX(buf)
      ? WIN32_NON_UNC_PREFIX_LEN
      : 0
  ;

  ptr = buf + len;
  if ( WIN32_IS_UNC_PREFIX(buf) ) {
    ptr[0] = '/';
    ptr[1] = '/';
  }

  res = strdup(ptr);

  free(buf);

  return res;
}

# if _WIN32_WINNT >= 0x0600

/* Get full path name WITH symlinks resolution.
   It also converts all forward slashes to back slashes.
*/
char* get_final_path_name(HANDLE fh) {
  DWORD len;
  char *buf, *ptr, *res;

  /* determining the required buffer size.
     from the  man: `If the function fails because lpszFilePath is too
     small to hold the string plus the terminating null character,
     the return value is the required buffer size, in TCHARs. This
     value _includes_the_size_of_the_terminating_null_character_`.
     but in my testcase I have path with 26 chars, the function
     returns 26 also, ie without the trailing zero-char...
  */
  len = GetFinalPathNameByHandle(
     fh
    ,NULL
    ,0
    ,FILE_NAME_NORMALIZED | VOLUME_NAME_DOS
  );

  if ( len == 0 )
    return NULL;

  len += 1; /* for zero-char */
  buf = (char *)malloc(len);

  /* no point to check the result again */
  len = GetFinalPathNameByHandle(
     fh
    ,buf
    ,len
    ,FILE_NAME_NORMALIZED | VOLUME_NAME_DOS
  );
  buf[len] = 0;

  /* replace slashes */
  WIN32_REPLACE_SLASHES(buf, len);

  /* calculate offset based on prefix type */
  len = WIN32_IS_UNC_PREFIX(buf)
    ? (WIN32_UNC_PREFIX_LEN - 2)
    : WIN32_IS_NON_UNC_PREFIX(buf)
      ? WIN32_NON_UNC_PREFIX_LEN
      : 0
  ;

  ptr = buf + len;
  if ( WIN32_IS_UNC_PREFIX(buf) ) {
    ptr[0] = '/';
    ptr[1] = '/';
  }

  res = strdup(ptr);

  free(buf);

  return res;
}

# endif // _WIN32_WINNT >= 0x0600

# endif // _WIN32
#endif

char *
lrealpath (const char *filename)
{
  /* Method 1: The system has a compile time upper bound on a filename
     path.  Use that and realpath() to canonicalize the name.  This is
     the most common case.  Note that, if there isn't a compile time
     upper bound, you want to avoid realpath() at all costs.  */
#if defined(REALPATH_LIMIT)
  {
    char buf[REALPATH_LIMIT];
    const char *rp = realpath (filename, buf);
    if (rp == NULL)
      rp = filename;
    return strdup (rp);
  }
#endif /* REALPATH_LIMIT */

  /* Method 2: The host system (i.e., GNU) has the function
     canonicalize_file_name() which malloc's a chunk of memory and
     returns that, use that.  */
#if defined(HAVE_CANONICALIZE_FILE_NAME)
  {
    char *rp = canonicalize_file_name (filename);
    if (rp == NULL)
      return strdup (filename);
    else
      return rp;
  }
#endif

  /* Method 3: Now we're getting desperate!  The system doesn't have a
     compile time buffer size and no alternative function.  Query the
     OS, using pathconf(), for the buffer limit.  Care is needed
     though, some systems do not limit PATH_MAX (return -1 for
     pathconf()) making it impossible to pass a correctly sized buffer
     to realpath() (it could always overflow).  On those systems, we
     skip this.  */
#if defined (HAVE_REALPATH) && defined (HAVE_UNISTD_H)
  {
    /* Find out the max path size.  */
    long path_max = pathconf ("/", _PC_PATH_MAX);
    if (path_max > 0)
      {
	/* PATH_MAX is bounded.  */
	char *buf, *rp, *ret;
	buf = (char *) malloc (path_max);
	if (buf == NULL)
	  return NULL;
	rp = realpath (filename, buf);
	ret = strdup (rp ? rp : filename);
	free (buf);
	return ret;
      }
  }
#endif

  /* The MS Windows method */
#if defined (_WIN32)
  {
    char *res;

    /* For Windows Vista and greater */
#if _WIN32_WINNT >= 0x0600

    /* For some reason the function receives just empty `filename`, but not NULL.
       What should we do in that case?
       According to `strdup()` implementation
         (https://elixir.bootlin.com/glibc/latest/source/string/strdup.c)
       it will alloc 1 byte even for empty but non NULL string.
       OK, will use `strdup()` for that case.
    */
    if ( 0 == strlen(filename) )
      return strdup(filename);

    HANDLE fh = CreateFile(
       filename
      ,FILE_READ_ATTRIBUTES
      ,FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE
      ,NULL
      ,OPEN_EXISTING
      ,FILE_FLAG_BACKUP_SEMANTICS
      ,NULL
    );

    if ( fh == INVALID_HANDLE_VALUE ) {
      res = get_full_path_name(filename);
    } else {
      res = get_final_path_name(fh);
      CloseHandle(fh);

      if ( !res )
        res = get_full_path_name(filename);
    }

#else

    /* For Windows XP */
    res = get_full_path_name(filename);

#endif // _WIN32_WINNT >= 0x0600

    return res;
  }
#endif // _WIN32
}

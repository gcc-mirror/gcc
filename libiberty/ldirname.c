/* Libiberty dirname.  Like dirname, but is not overridden by the
   system C library.
   Copyright (C) 2025 Free Software Foundation, Inc.

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
not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

/*

@deftypefn Replacement {char*} ldirname (const char *@var{name})

Given a pointer to a string containing a typical pathname
(@samp{/usr/src/cmd/ls/ls.c} for example), returns a string containing the
passed string up to, but not including, the final directory separator.

If the given pathname doesn't contain a directory separator then this funtion
returns the empty string; this includes an empty given pathname.  @code{NULL}
is returned on memory allocation error.

@end deftypefn

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "ansidecl.h"
#include "libiberty.h"
#include "safe-ctype.h"
#include "filenames.h"

/* For malloc.  */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* For memcpy.  */
# if HAVE_STRING_H
#  include <string.h>
# else
#  if HAVE_STRINGS_H
#   include <strings.h>
#  endif
# endif

#define LDIRNAME(FPREFIX,DIRSEP)					\
  char *FPREFIX##_ldirname (const char *name)				\
  {									\
    /* Note that lbasename guarantees that the returned */		\
    /* pointer lies within the passed string.  */			\
    const char *basename = FPREFIX##_lbasename (name);			\
    size_t size = basename - name;					\
    char *res = NULL;							\
    									\
    res = (char*) malloc (size + 1);					\
    if (res != NULL)							\
      {									\
	if (size > 0)							\
	  {								\
	    if (IS_DIR_SEPARATOR_1 ((DIRSEP),name[size - 1]))		\
	      size -= 1;						\
	    memcpy (res, name, size);					\
	  }								\
	res[size] = '\0';						\
      }									\
    									\
    return res;								\
  }

LDIRNAME(dos,1)
LDIRNAME(unix,0)

char *
ldirname (const char *name)
{
#if defined (HAVE_DOS_BASED_FILE_SYSTEM)
  return dos_ldirname (name);
#else
  return unix_ldirname (name);
#endif
}

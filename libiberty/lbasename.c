/* Libiberty basename.  Like basename, but is not overridden by the
   system C library.
   Copyright (C) 2001 Free Software Foundation, Inc.

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
NAME
	lbasename -- return pointer to last component of a pathname

SYNOPSIS
	char *lbasename (const char *name)

DESCRIPTION
	Given a pointer to a string containing a typical pathname
	(/usr/src/cmd/ls/ls.c for example), returns a pointer to the
	last component of the pathname ("ls.c" in this case).  The
	returned pointer is guaranteed to lie within the original
	string.  This latter fact is not true of many vendor C
	libraries, which return special strings or modify the passed
	strings for particular input.

	In particular, the empty string returns the same empty string,
	and a path ending in '/' returns the empty string after it.
*/

#include "ansidecl.h"
#include "libiberty.h"
#include "safe-ctype.h"

#ifndef DIR_SEPARATOR
#  define DIR_SEPARATOR '/'
#endif

#if defined (_WIN32) || defined (__MSDOS__) \
    || defined (__DJGPP__) || defined (__OS2__)
#  define HAVE_DOS_BASED_FILE_SYSTEM
#  ifndef DIR_SEPARATOR_2 
#    define DIR_SEPARATOR_2 '\\'
#  endif
#endif

/* Define IS_DIR_SEPARATOR. VMS uses '::', ':', '[...]' and '<...>' to
   separate the different components of a file specification.  It's a
   bit of a stretch to call ':', ']' and '>' directory separators, so
   just define the test to find the file name component.  */
#ifdef VMS
#  define IS_DIR_SEPARATOR(ch) ((ch) == ':' || (ch) == ']' || (ch) == '>')
#else
#  ifndef DIR_SEPARATOR_2
#    define IS_DIR_SEPARATOR(ch) ((ch) == DIR_SEPARATOR)
#  else
#    define IS_DIR_SEPARATOR(ch) \
	(((ch) == DIR_SEPARATOR) || ((ch) == DIR_SEPARATOR_2))
#  endif
#endif

char *
lbasename (name)
     const char *name;
{
  const char *base;

#if defined (HAVE_DOS_BASED_FILE_SYSTEM)
  /* Skip over a possible disk name.  */
  if (ISALPHA (name[0]) && name[1] == ':') 
    name += 2;
#endif

  for (base = name; *name; name++)
    if (IS_DIR_SEPARATOR (*name))
      base = name + 1;

  return (char *) base;
}

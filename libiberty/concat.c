/* Concatenate variable number of strings.
   Copyright (C) 1991, 1994 Free Software Foundation, Inc.
   Written by Fred Fish @ Cygnus Support

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

	concat -- concatenate a variable number of strings

SYNOPSIS

	#include <varargs.h>

	char *concat (s1, s2, s3, ..., NULL)

DESCRIPTION

	Concatenate a variable number of strings and return the result
	in freshly malloc'd memory.

	Returns NULL if insufficient memory is available.  The argument
	list is terminated by the first NULL pointer encountered.  Pointers
	to empty strings are ignored.

NOTES

	This function uses xmalloc() which is expected to be a front end
	function to malloc() that deals with low memory situations.  In
	typical use, if malloc() returns NULL then xmalloc() diverts to an
	error handler routine which never returns, and thus xmalloc will
	never return a NULL pointer.  If the client application wishes to
	deal with low memory situations itself, it should supply an xmalloc
	that just directly invokes malloc and blindly returns whatever
	malloc returns.
*/


#include "ansidecl.h"
#include "libiberty.h"

#ifdef ANSI_PROTOTYPES
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifdef __STDC__
#include <stddef.h>
extern size_t strlen (const char *s);
#else
extern int strlen ();
#endif

#define NULLP (char *)0

/* VARARGS */
#ifdef ANSI_PROTOTYPES
char *
concat (const char *first, ...)
#else
char *
concat (va_alist)
     va_dcl
#endif
{
  register int length;
  register char *newstr;
  register char *end;
  register const char *arg;
  va_list args;
#ifndef ANSI_PROTOTYPES
  const char *first;
#endif

  /* First compute the size of the result and get sufficient memory. */

#ifdef ANSI_PROTOTYPES
  va_start (args, first);
#else
  va_start (args);
  first = va_arg (args, const char *);
#endif

  if (first == NULLP)
    length = 0;
  else
    {
      length = strlen (first);
      while ((arg = va_arg (args, const char *)) != NULLP)
	{
	  length += strlen (arg);
	}
    }
  newstr = (char *) xmalloc (length + 1);
  va_end (args);

  /* Now copy the individual pieces to the result string. */

  if (newstr != NULLP)
    {
#ifdef ANSI_PROTOTYPES
      va_start (args, first);
#else
      va_start (args);
      first = va_arg (args, const char *);
#endif
      end = newstr;
      if (first != NULLP)
	{
	  arg = first;
	  while (*arg)
	    {
	      *end++ = *arg++;
	    }
	  while ((arg = va_arg (args, const char *)) != NULLP)
	    {
	      while (*arg)
		{
		  *end++ = *arg++;
		}
	    }
	}
      *end = '\000';
      va_end (args);
    }

  return (newstr);
}

#ifdef MAIN

/* Simple little test driver. */

#include <stdio.h>

int
main ()
{
  printf ("\"\" = \"%s\"\n", concat (NULLP));
  printf ("\"a\" = \"%s\"\n", concat ("a", NULLP));
  printf ("\"ab\" = \"%s\"\n", concat ("a", "b", NULLP));
  printf ("\"abc\" = \"%s\"\n", concat ("a", "b", "c", NULLP));
  printf ("\"abcd\" = \"%s\"\n", concat ("ab", "cd", NULLP));
  printf ("\"abcde\" = \"%s\"\n", concat ("ab", "c", "de", NULLP));
  printf ("\"abcdef\" = \"%s\"\n", concat ("", "a", "", "bcd", "ef", NULLP));
  return 0;
}

#endif

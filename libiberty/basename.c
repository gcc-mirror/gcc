/* Return the basename of a pathname.
   This file is in the public domain. */

/*
NAME
	basename -- return pointer to last component of a pathname

SYNOPSIS
	char *basename (const char *name)

DESCRIPTION
	Given a pointer to a string containing a typical pathname
	(/usr/src/cmd/ls/ls.c for example), returns a pointer to the
	last component of the pathname ("ls.c" in this case).

BUGS
	Presumes a UNIX style path with UNIX style separators.
*/

#include "ansidecl.h"
#include "libiberty.h"

#include "config.h"

#ifdef NEED_basename

char *
basename (name)
     const char *name;
{
  const char *base = name;

  while (*name)
    {
      if (*name++ == '/')
	{
	  base = name;
	}
    }
  return (char *) base;
}

#endif

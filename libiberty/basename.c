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
	Presumes a UNIX or DOS/Windows style path with UNIX or DOS/Windows 
	style separators.
*/

#include "ansidecl.h"
#include "libiberty.h"
#include "safe-ctype.h"

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

#if defined (_WIN32) || defined (__MSDOS__) || defined (__DJGPP__) || \
  defined (__OS2__)
#define HAVE_DOS_BASED_FILE_SYSTEM
#ifndef DIR_SEPARATOR_2 
#define DIR_SEPARATOR_2 '\\'
#endif
#endif

/* Define IS_DIR_SEPARATOR.  */
#ifndef DIR_SEPARATOR_2
# define IS_DIR_SEPARATOR(ch) ((ch) == DIR_SEPARATOR)
#else /* DIR_SEPARATOR_2 */
# define IS_DIR_SEPARATOR(ch) \
	(((ch) == DIR_SEPARATOR) || ((ch) == DIR_SEPARATOR_2))
#endif /* DIR_SEPARATOR_2 */

char *
basename (name)
     const char *name;
{
  const char *base;

#if defined (HAVE_DOS_BASED_FILE_SYSTEM)
  /* Skip over the disk name in MSDOS pathnames. */
  if (ISALPHA (name[0]) && name[1] == ':') 
    name += 2;
#endif

  for (base = name; *name; name++)
    {
      if (IS_DIR_SEPARATOR (*name))
	{
	  base = name + 1;
	}
    }
  return (char *) base;
}


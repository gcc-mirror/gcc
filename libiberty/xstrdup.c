/* xstrdup.c -- Duplicate a string in memory, using xmalloc.
   This trivial function is in the public domain.
   Ian Lance Taylor, Cygnus Support, December 1995.  */

#include "ansidecl.h"
#include "libiberty.h"

char *
xstrdup (s)
     const char *s;
{
  char *ret;

  ret = xmalloc (strlen (s) + 1);
  strcpy (ret, s);
  return ret;
}

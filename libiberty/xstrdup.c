/* xstrdup.c -- Duplicate a string in memory, using xmalloc.
   This trivial function is in the public domain.
   Ian Lance Taylor, Cygnus Support, December 1995.  */

#include <sys/types.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include "ansidecl.h"
#include "libiberty.h"

char *
xstrdup (s)
  const char *s;
{
  register size_t len = strlen (s) + 1;
  register char *ret = xmalloc (len);
  memcpy (ret, s, len);
  return ret;
}

#include "f2c.h"
#undef abs
#undef min
#undef max
#include <stdio.h>

static integer memfailure = 3;

#include <stdlib.h>
extern void G77_exit_0 (integer *);

char *
F77_aloc (integer Len, char *whence)
{
  char *rv;
  unsigned int uLen = (unsigned int) Len;	/* for K&R C */

  if (!(rv = (char *) malloc (uLen)))
    {
      fprintf (stderr, "malloc(%u) failure in %s\n", uLen, whence);
      G77_exit_0 (&memfailure);
    }
  return rv;
}

#include "f2c.h"

/*
 * subroutine getarg(k, c)
 * returns the kth unix command argument in fortran character
 * variable argument c
*/

void
G77_getarg_0 (ftnint * n, register char *s, ftnlen ls)
{
  extern int f__xargc;
  extern char **f__xargv;
  register char *t;
  register int i;

  if (*n >= 0 && *n < f__xargc)
    t = f__xargv[*n];
  else
    t = "";
  for (i = 0; i < ls && *t != '\0'; ++i)
    *s++ = *t++;
  for (; i < ls; ++i)
    *s++ = ' ';
}

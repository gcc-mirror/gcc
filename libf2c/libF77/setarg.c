/* Set up the global argc/argv info for use by getarg_, iargc_, and
   g77's inlined intrinsic equivalents.  */

#include <stdlib.h>

int f__xargc;
char **f__xargv;

void
f_setarg (int argc, char **argv)
{
  f__xargc = argc;
  f__xargv = argv;
}

/* Set up the global argc/argv info for use by getarg_, iargc_, and
   g77's inlined intrinsic equivalents.  */

#undef VOID
#include <stdlib.h>

#ifndef VOID
#define VOID void
#endif

int f__xargc;
char **f__xargv;

#ifdef __cplusplus
	}
#endif

 void
f_setarg(int argc, char **argv)
{
f__xargc = argc;
f__xargv = argv;
}

/* Set up the global argc/argv info for use by getarg_, iargc_, and
   g77's inlined intrinsic equivalents.  */

#ifndef KR_headers
#undef VOID
#include <stdlib.h>
#endif

#ifndef VOID
#define VOID void
#endif

int f__xargc;
char **f__xargv;

#ifdef __cplusplus
	}
#endif

 void
#ifdef KR_headers
f_setarg(argc, argv) int argc; char **argv;
#else
f_setarg(int argc, char **argv)
#endif
{
f__xargc = argc;
f__xargv = argv;
}

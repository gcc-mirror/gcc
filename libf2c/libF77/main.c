/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */

#include <stdio.h>
#include "signal1.h"

#ifndef KR_headers
#undef VOID
#include <stdlib.h>
#endif

#ifndef VOID
#define VOID void
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef NO__STDC
#define ONEXIT onexit
extern VOID f_exit();
#else
#ifndef KR_headers
extern void f_exit(void);
#ifndef NO_ONEXIT
#define ONEXIT atexit
extern int atexit(void (*)(void));
#endif
#else
#ifndef NO_ONEXIT
#define ONEXIT onexit
extern VOID f_exit();
#endif
#endif
#endif

#ifdef KR_headers
extern VOID f_init();
extern int MAIN__();
#else
extern void f_init(void);
extern int MAIN__(void);
#endif

#ifdef __cplusplus
	}
#endif

#ifdef KR_headers
main(argc, argv) int argc; char **argv;
#else
main(int argc, char **argv)
#endif
{
f_setarg(argc, argv);
f_setsig();
f_init();
#ifndef NO_ONEXIT
ONEXIT(f_exit);
#endif
MAIN__();
#ifdef NO_ONEXIT
f_exit();
#endif
exit(0);	/* exit(0) rather than return(0) to bypass Cray bug */
return 0;	/* For compilers that complain of missing return values; */
		/* others will complain that this is unreachable code. */
}

/* STARTUP PROCEDURE FOR UNIX FORTRAN PROGRAMS */

#include <stdio.h>
#include "signal1.h"

#include <stdlib.h>

extern void f_exit (void);
#ifndef NO_ONEXIT
#define ONEXIT atexit
extern int atexit (void (*)(void));
#endif

extern void f_init (void);
extern int MAIN__ (void);
extern void f_setarg (int, char **);
extern void f_setsig (void);

int
main (int argc, char **argv)
{
  f_setarg (argc, argv);
  f_setsig ();
  f_init ();
#ifndef NO_ONEXIT
  ONEXIT (f_exit);
#endif
  MAIN__ ();
#ifdef NO_ONEXIT
  f_exit ();
#endif
  exit (0);			/* exit(0) rather than return(0) to bypass Cray bug */
  return 0;			/* For compilers that complain of missing return values; */
  /* others will complain that this is unreachable code. */
}

/* Version of sigsetmask.c
   Written by Steve Chamberlain (sac@cygnus.com).
   Contributed by Cygnus Support.
   This file is in the public doamin. */

/* Set the current signal mask to the set provided, and return the 
   previous value */

#define _POSIX_SOURCE
#include <ansidecl.h>
/* Including <sys/types.h> seems to be needed by ISC. */
#include <sys/types.h>
#include <signal.h>

#ifdef SIG_SETMASK
int
DEFUN(sigsetmask,(set),
      int set)
{
    sigset_t new;
    sigset_t old;
    
    sigemptyset (&new);
    if (set != 0) {
      abort();	/* FIXME, we don't know how to translate old mask to new */
    }
    sigprocmask(SIG_SETMASK, &new, &old);
    return 1;	/* FIXME, we always return 1 as old value.  */
}
#endif

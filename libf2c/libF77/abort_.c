#include <stdio.h>
#include "f2c.h"

#ifdef KR_headers
extern VOID sig_die();

int G77_abort_0 ()
#else
extern void sig_die(char*,int);

int G77_abort_0 (void)
#endif
{
sig_die("Fortran abort routine called", 1);
return 0;	/* not reached */
}

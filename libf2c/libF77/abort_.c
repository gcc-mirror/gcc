#include <stdio.h>
#include "f2c.h"

extern void sig_die (char *, int);

int
G77_abort_0 (void)
{
  sig_die ("Fortran abort routine called", 1);
  return 0;			/* not reached */
}

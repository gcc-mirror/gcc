/* This is needed for fpu-glibc.h, before all other includes */
#ifdef HAVE_FENV_H
#define _GNU_SOURCE
#endif

#include "libgfortran.h"

/* We include the platform-dependent code.  */
#include "fpu-target.h"

/* Function called by the front-end to tell us
   when a FPE should be raised.  */
extern void set_fpe (int);
export_proto(set_fpe);

void
set_fpe (int exceptions)
{
  options.fpe = exceptions;
  set_fpu ();
}

/* This should be replaced by whatever namespace-clean
   version of strerror you have available. */

#include "libioP.h"
extern char *strerror __P ((int));

char *
DEFUN(_IO_strerror, (errnum),
      int errnum)
{
  return strerror(errnum);
}

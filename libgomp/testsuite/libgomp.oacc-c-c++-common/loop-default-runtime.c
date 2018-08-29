/* { dg-set-target-env-var GOMP_OPENACC_DIM "8::" } */

#include "loop-default.h"
#include <stdlib.h>

int
main ()
{
  if (check_gang (8) != 0)
    abort ();

  return 0;
}

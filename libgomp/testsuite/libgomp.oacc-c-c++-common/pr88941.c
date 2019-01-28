/* { dg-do run } */

#include <stdlib.h>

int
main (void)
{

#pragma acc parallel async
  ;

  /* no #pragma acc wait */
  return 0;
}


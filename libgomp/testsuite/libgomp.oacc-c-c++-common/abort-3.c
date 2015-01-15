/* { dg-do run } */
/* { dg-shouldfail "" { *-*-* } { "*" } { "" } } */

#include <stdlib.h>

int
main (void)
{

#pragma acc kernels
  {
    abort ();
  }

  return 0;
}


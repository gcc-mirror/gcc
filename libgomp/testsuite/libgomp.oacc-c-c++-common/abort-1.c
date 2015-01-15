/* { dg-do run } */
/* { dg-shouldfail "" { *-*-* } { "*" } { "" } } */

#include <stdlib.h>

int
main (void)
{

#pragma acc parallel
  {
    abort ();
  }

  return 0;
}


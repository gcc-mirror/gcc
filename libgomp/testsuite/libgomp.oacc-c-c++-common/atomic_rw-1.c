/* { dg-do run } */

#include <stdlib.h>

int
main(int argc, char **argv)
{
  int v1, v2;
  int x;

  x = 99;

#pragma acc parallel copy (v1, v2, x)
  {

#pragma acc atomic read
    v1 = x;

#pragma acc atomic write
    x = 32;

#pragma acc atomic read
    v2 = x;

  }

  if (v1 != 99)
    abort ();

  if (v2 != 32)
    abort ();

  return 0;
}

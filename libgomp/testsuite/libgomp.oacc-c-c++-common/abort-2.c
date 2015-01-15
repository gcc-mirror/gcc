/* { dg-do run } */

#include <stdlib.h>

int
main (int argc, char **argv)
{

#pragma acc parallel
  {
    if (argc != 1)
      abort ();
  }

  return 0;
}


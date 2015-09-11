/* { dg-do run } */
/* { dg-additional-options "-flto" { target lto } } */

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


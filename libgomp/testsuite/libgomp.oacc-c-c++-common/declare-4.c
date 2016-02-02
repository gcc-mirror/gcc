/* { dg-do run  { target openacc_nvidia_accel_selected } } */

#include <stdlib.h>
#include <openacc.h>

float b;
#pragma acc declare create (b)

#pragma acc routine
int
func (int a)
{
  b = a + 1;

  return b;
}

int
main (int argc, char **argv)
{
  float a;

  a = 2.0;

#pragma acc parallel copy (a)
  {
    b = a;
    a = 1.0;
    a = a + b;
  }

  if (a != 3.0)
    abort ();

  a = func (a);

  if (a != 4.0)
    abort ();

  return 0;
}

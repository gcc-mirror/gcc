/* { dg-do run { target openacc_nvidia_accel_selected } } */

#include <stdlib.h>

#define N 16

float c[N];
#pragma acc declare device_resident (c)

#pragma acc routine
float
subr2 (float a)
{
  int i;

  for (i = 0; i < N; i++)
    c[i] = 2.0;

  for (i = 0; i < N; i++)
    a += c[i];

  return a;
}

float b[N];
#pragma acc declare copyin (b)

#pragma acc routine
float
subr1 (float a)
{
  int i;

  for (i = 0; i < N; i++)
    a += b[i];

  return a;
}

int
main (int argc, char **argv)
{
  float a;
  int i;

  for (i = 0; i < 16; i++)
    b[i] = 1.0;

  a = 0.0;

  a = subr1 (a);

  if (a != 16.0)
    abort ();

  a = 0.0;

  a = subr2 (a);

  if (a != 32.0)
    abort ();

  return 0;
}

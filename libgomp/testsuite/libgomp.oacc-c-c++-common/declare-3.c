#include <stdlib.h>
#include <openacc.h>

float *b;
#pragma acc declare deviceptr (b)

#pragma acc routine
float *
subr2 (void)
{
  return b;
}

float
subr1 (float a)
{
  float b;
#pragma acc declare present_or_copy (b)
  float c;
#pragma acc declare present_or_copyin (c)
  float d;
#pragma acc declare present_or_create (d)
  float e;
#pragma acc declare present_or_copyout (e)

#pragma acc parallel copy (a)
  {
    b = a;
    c = b;
    d = c;
    e = d;
    a = e;
  }

  return a;
}

int
main (int argc, char **argv)
{
  float a;
  float *c;

  a = 2.0;

  a = subr1 (a);

  if (a != 2.0)
    abort ();

  b = (float *) acc_malloc (sizeof (float));

  c = subr2 ();

  if (b != c)
    abort ();

  return 0;
}

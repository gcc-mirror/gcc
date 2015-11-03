/* This test exercises combined directives.  */

/* { dg-do run } */

#include <stdlib.h>

int
main (int argc, char **argv)
{
  const int N = 32;
  float a[N], b[N];
  int i;

  for (i = 0; i < N; i++)
    {
      a[i] = 1.0;
      b[i] = 0.0;
    }

#pragma acc parallel loop copy (a[0:N]) copy (b[0:N])
  for (i = 0; i < N; i++)
    {
      b[i] = 2.0;
      a[i] = a[i] + b[i];
    }

  for (i = 0; i < N; i++)
    {
      if (a[i] != 3.0)
	abort ();

      if (b[i] != 2.0)
	abort ();
    }

#pragma acc kernels loop copy (a[0:N]) copy (b[0:N])
  for (i = 0; i < N; i++)
    {
      b[i] = 3.0;
      a[i] = a[i] + b[i];
    }

  for (i = 0; i < N; i++)
    {
      if (a[i] != 6.0)
	abort ();

      if (b[i] != 3.0)
	abort ();
    }

  return 0;

} 

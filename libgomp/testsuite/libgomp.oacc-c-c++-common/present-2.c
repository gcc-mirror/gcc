/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <openacc.h>
#include <stdlib.h>

int
main (int argc, char **argv)
{
  int N = 8;
  float *a, *b;
  int i;

  a = (float *) malloc (N * sizeof (float));
  b = (float *) malloc (N * sizeof (float));

  for (i = 0; i < N; i++)
    {
      a[i] = 4.0;
      b[i] = 0.0;
    }

#pragma acc data copyin(a[0:N]) copyout(b[0:N])
  {

#pragma acc parallel present(a[0:N])
    {
      int ii;

      for (ii = 0; ii < N; ii++)
	{
	  b[ii] = a[ii];
	}
    }

  }

  for (i = 0; i < N; i++)
    {
      if (a[i] != 4.0)
	abort ();

      if (b[i] != 4.0)
	abort ();
    }

  return 0;
}

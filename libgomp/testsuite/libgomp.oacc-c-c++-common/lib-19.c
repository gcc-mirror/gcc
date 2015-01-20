/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <string.h>
#include <stdlib.h>
#include <openacc.h>

#include <stdio.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i;
  unsigned char *h[N];

  for (i = 0; i < N; i++)
    {
      int j;
      unsigned char *p;

      h[i] = (unsigned char *) malloc (N);
      p = h[i];

      for (j = 0; j < N; j++)
	{
	  p[j] = i;
	}

      (void) acc_copyin (p, N);
    }

  for (i = 0; i < N; i++)
    {
      memset (h[i], 0, i);
    }

  for (i = 0; i < N; i++)
    {
      int j;
      unsigned char *p;

      acc_copyout (h[i], N);

      p = h[i];

      for (j = 0; j < N; j++)
	{
	  if (p[j] != i)
	    abort ();
	}
    }

  for (i = 0; i < N; i++)
    {
      free (h[i]);
    }

  return 0;
}

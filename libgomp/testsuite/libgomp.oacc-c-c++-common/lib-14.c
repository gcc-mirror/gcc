/* Check acc_is_present.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdlib.h>
#include <openacc.h>

#include <stdio.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i;
  unsigned char *h;
  void *d;

  h = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h[i] = i;
    }

  d = acc_copyin (h, N);

  if (acc_is_present (h, 1) != 1)
    abort ();

  if (acc_is_present (h + N - 1, 1) != 1)
    abort ();

  if (acc_is_present (h - 1, 1) != 0)
    abort ();

  if (acc_is_present (h + N, 1) != 0)
    abort ();

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, 1) != 1)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, N - i) != 1)
	abort ();
    }

  acc_delete (h, N);

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, N - i) != 0)
	abort ();
    }


  free (h);

  return 0;
}

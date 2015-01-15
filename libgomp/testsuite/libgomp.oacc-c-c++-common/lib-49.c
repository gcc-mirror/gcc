/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <string.h>
#include <stdlib.h>
#include <openacc.h>

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
  if (!d)
    abort ();

  memset (&h[0], 0, N);

  acc_update_self (h, N - 2);

  for (i = 0; i < N - 2; i++)
    {
      if (h[i] != i)
	abort ();
    }

  for (i = N - 2; i < N; i++)
    {
      if (h[i] != 0)
	abort ();
    }

  acc_delete (h, N);

  free (h);

  return 0;
}

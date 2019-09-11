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
  int async = 8;
  unsigned char *h;

  h = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h[i] = i;
    }

  acc_copyin_async (h, N, async);

  acc_wait (async);

  memset (h, 0, N);

  acc_copyout_async (h, N, async + 1);

  acc_wait (async + 1);

  for (i = 0; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  free (h);

  return 0;
}

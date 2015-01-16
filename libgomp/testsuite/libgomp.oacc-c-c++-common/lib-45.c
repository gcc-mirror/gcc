/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

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

  for (i = 0; i < N; i++)
    {
      h[i] = 0xab;
    }

  acc_update_device (h, N - 2);

  acc_copyout (h, N);

  for (i = 0; i < N - 2; i++)
    {
      if (h[i] != 0xab)
	abort ();
    }

  for (i = N - 2; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  free (h);

  return 0;
}

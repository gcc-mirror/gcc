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
  void *d1, *d2;

  h = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h[i] = i;
    }

  d1 = acc_present_or_copyin (h, N);
  if (!d1)
    abort ();

  for (i = 0; i < N; i++)
    {
      h[i] = 0xab;
    }

  d2 = acc_present_or_copyin (h, N);
  if (!d2)
    abort ();

  if (d1 != d2)
    abort ();

  memset (&h[0], 0, N);

  acc_copyout (h, N);

  for (i = 0; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  d2 = acc_pcopyin (h, N);
  if (!d2)
    abort ();

  acc_copyout (h, N);

  for (i = 0; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  free (h);

  return 0;
}

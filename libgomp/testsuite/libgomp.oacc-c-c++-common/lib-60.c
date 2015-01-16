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

  d = acc_malloc (N);

  acc_memcpy_to_device (d, h, N);

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, 1) != 0)
	abort ();
    }

  memset (&h[0], 0, N);

  acc_memcpy_from_device (h, d, N);

  for (i = 0; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  for (i = 0; i < N; i++)
    {
      if (acc_is_present (h + i, 1) != 0)
	abort ();
    }

  acc_free (d);

  free (h);

  return 0;
}

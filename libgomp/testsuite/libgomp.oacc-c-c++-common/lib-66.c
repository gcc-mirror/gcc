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

  acc_init (acc_device_default);

  h = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h[i] = i;
    }

  d = acc_malloc (N);

  acc_memcpy_to_device (d, h, N);

  memset (&h[0], 0, N);

  acc_memcpy_to_device (d, h, 0);

  acc_memcpy_from_device (h, d, N);

  for (i = 0; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  acc_free (d);

  free (h);

  acc_shutdown (acc_device_default);

  return 0;
}

/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <string.h>
#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i, q = 5;
  unsigned char *h, *g;
  void *d;

  h = (unsigned char *) malloc (N);
  g = (unsigned char *) malloc (N);
  for (i = 0; i < N; i++)
    {
      g[i] = i;
    }

  acc_create_async (h, N, q);

  acc_memcpy_to_device_async (acc_deviceptr (h), g, N, q);
  memset (&h[0], 0, N);

  acc_wait (q);

  acc_update_self_async (h, N, q + 1);
  acc_delete_async (h, N, q + 1);

  acc_wait (q + 1);

  for (i = 0; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  free (h);
  free (g);

  return 0;
}

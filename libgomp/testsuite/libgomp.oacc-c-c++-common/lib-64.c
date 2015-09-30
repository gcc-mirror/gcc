/* { dg-do run { target openacc_nvidia_accel_selected } } */

#include <stdio.h>
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

  fprintf (stderr, "CheCKpOInT\n");
  acc_memcpy_to_device (d, 0, N);

  memset (&h[0], 0, N);

  acc_memcpy_from_device (h, d, N);

  for (i = 0; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  acc_free (d);

  free (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "invalid host address" } */
/* { dg-shouldfail "" } */

/* Exercise acc_update_device with a NULL data address on nvidia targets.  */

/* { dg-do run { target openacc_nvidia_accel_selected } } */

#include <stdio.h>
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

  fprintf (stderr, "CheCKpOInT\n");
  acc_update_device (0, N);

  acc_copyout (h, N);

  for (i = 0; i < N; i++)
    {
      if (h[i] != 0xab)
	abort ();
    }

  free (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\\\[\[^\n\r]*,256\\\] is not mapped" } */
/* { dg-shouldfail "" } */

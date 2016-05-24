/* Exercise acc_update_self with a size zero data mapping on nvidia targets.  */

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

  d = acc_copyin (h, N);
  if (!d)
    abort ();

  memset (&h[0], 0, N);

  fprintf (stderr, "CheCKpOInT\n");
  acc_update_self (h, 0);

  for (i = 0; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  acc_delete (h, N);

  free (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\\\[\[0-9a-fA-FxX\]+,0\\\] is not mapped" } */
/* { dg-shouldfail "" } */

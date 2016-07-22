/* Exercise an invalid partial acc_delete on nvidia targets.  */

/* { dg-do run { target openacc_nvidia_accel_selected } } */

#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  unsigned char *h;
  void *d;

  h = (unsigned char *) malloc (N);

  d = acc_create (h, N);
  if (!d)
    abort ();

  fprintf (stderr, "CheCKpOInT\n");
  acc_delete (h, N - 2);

  free (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\\\[\[0-9a-fA-FxX\]+,256\\\] surrounds2 \\\[\[0-9a-fA-FxX\]+,\\\+254\\\]" } */
/* { dg-shouldfail "" } */

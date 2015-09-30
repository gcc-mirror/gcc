/* { dg-do run } */

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

  fprintf (stderr, "CheCKpOInT\n");
  d = acc_create (h, 0);
  if (!d)
    abort ();

  acc_delete (h, N);

  free (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\\\[\[0-9a-fA-FxX\]+,\\\+0\\\] is a bad range" } */
/* { dg-shouldfail "" } */

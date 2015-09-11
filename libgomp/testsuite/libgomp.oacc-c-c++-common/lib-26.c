/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  unsigned char *h;
  void *d;

  h = (unsigned char *) malloc (N);

  d = acc_create (h, 0);
  if (!d)
    abort ();

  acc_delete (h, N);

  free (h);

  return 0;
}

/* { dg-output "\\\[\[0-9a-fA-FxX\]+,\\\+0\\\] is a bad range" } */
/* { dg-shouldfail "" } */

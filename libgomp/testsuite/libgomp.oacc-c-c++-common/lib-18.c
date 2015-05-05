/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

#include <stdio.h>

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

  acc_free (d);

  acc_copyout (h, N);

  free (h);

  return 0;
}

/* { dg-output "\\\[\[0-9a-fA-FxX\]+,256\\\] is not mapped" } */
/* { dg-shouldfail "" } */

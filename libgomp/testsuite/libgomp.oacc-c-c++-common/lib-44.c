/* { dg-do run } */

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

  acc_update_device (h, 0);

  acc_copyout (h, N);

  for (i = 0; i < N; i++)
    {
      if (h[i] != 0xab)
	abort ();
    }

  free (h);

  return 0;
}

/* { dg-output "\\\[\[0-9a-fA-FxX\]+,0\\\] is not mapped" } */
/* { dg-shouldfail "" } */

/* { dg-do run } */

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

/* { dg-shouldfail "libgomp: \[\h+,0\] is not mapped" } */

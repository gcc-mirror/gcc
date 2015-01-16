/* { dg-do run } */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
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

  d = acc_present_or_copyin (h, 0);
  if (!d)
    abort ();

  memset (&h[0], 0, N);

  acc_copyout (h, N);

  for (i = 0; i < N; i++)
    {
      if (h[i] != i)
	abort ();
    }

  free (h);

  return 0;
}

/* { dg-shouldfail "libgomp: \[\h+,\+0\] is a bad range" } */

/* { dg-do run } */

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

  fprintf (stderr, "CheCKpOInT\n");
  d = acc_present_or_copyin (0, N);
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

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\\\[\[^\n\r]*,\\\+256\\\] is a bad range" } */
/* { dg-shouldfail "" } */

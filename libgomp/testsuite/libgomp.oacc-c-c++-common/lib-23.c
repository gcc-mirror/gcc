/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  const int N = 256;
  int i;
  unsigned char *h1, *h2;

  h1 = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h1[i] = 0xab;
    }

  (void) acc_copyin (h1, N);

  h2 = (unsigned char *) malloc (N);

  for (i = 0; i < N; i++)
    {
      h2[i] = 0xde;
    }

  (void) acc_copyin (h2, N);

  acc_copyout (h1, N + N);

  free (h1);
  free (h2);

  return 0;
}

/* { dg-shouldfail "libgomp: \[\h+,256\] surrounds2 \[\h+,\+512\]" } */

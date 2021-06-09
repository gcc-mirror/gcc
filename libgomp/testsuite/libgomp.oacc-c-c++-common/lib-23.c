/* Exercise acc_copyin and acc_copyout.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
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

  fprintf (stderr, "CheCKpOInT\n");
  acc_copyout (h1, N + N);

  free (h1);
  free (h2);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "\\\[\[0-9a-fA-FxX\]+,\\\+512\\\] outside mapped block \\\[\[0-9a-fA-FxX\]+,\\\+256\\\]" } */
/* { dg-shouldfail "" } */

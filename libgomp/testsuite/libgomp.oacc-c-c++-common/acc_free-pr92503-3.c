/* Verify that we refuse 'acc_free', after '#pragma acc enter data create'.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>

int
main ()
{
  const int N = 108;

  char *h = (char *) malloc (N);
#pragma acc enter data create (h[0:N - 3])
  void *d = acc_deviceptr (h);
  if (!d)
    abort ();

  fprintf (stderr, "CheCKpOInT\n");
  acc_free (d);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" }
   { dg-output "refusing to free device memory space at \[0-9a-fA-FxX\]+ that is still mapped at \\\[\[0-9a-fA-FxX\]+,\\\+105\\\]" }
   { dg-shouldfail "" } */

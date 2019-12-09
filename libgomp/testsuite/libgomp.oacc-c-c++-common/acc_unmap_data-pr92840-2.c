/* Verify that we refuse 'acc_unmap_data', after '#pragma acc enter data create'.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>

int
main ()
{
  const int N = 101;

  char *h = (char *) malloc (N);
#pragma acc enter data create (h[0:N - 77])

  fprintf (stderr, "CheCKpOInT\n");
  acc_unmap_data (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "refusing to unmap block \\\[\[0-9a-fA-FxX\]+,\\\+24\\\] that has not been mapped by 'acc_map_data'" } */
/* { dg-shouldfail "" } */

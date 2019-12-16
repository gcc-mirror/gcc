/* Verify that we refuse 'acc_unmap_data', inside 'data'.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>

int
main ()
{
  const int N = 101;

  char *h = (char *) malloc (N);
#pragma acc data create (h[0:N - 55])
  {
    fprintf (stderr, "CheCKpOInT\n");
    acc_unmap_data (h);
  }

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "refusing to unmap block \\\[\[0-9a-fA-FxX\]+,\\\+46\\\] that has not been mapped by 'acc_map_data'" } */
/* { dg-shouldfail "" } */

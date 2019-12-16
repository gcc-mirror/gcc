/* Verify that we refuse 'acc_unmap_data', after 'acc_create'.  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>

int
main ()
{
  const int N = 101;

  char *h = (char *) malloc (N);
  void *d = acc_create (h, N - 3);
  if (!d)
    abort ();

  fprintf (stderr, "CheCKpOInT\n");
  acc_unmap_data (h);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "refusing to unmap block \\\[\[0-9a-fA-FxX\]+,\\\+98\\\] that has not been mapped by 'acc_map_data'" } */
/* { dg-shouldfail "" } */

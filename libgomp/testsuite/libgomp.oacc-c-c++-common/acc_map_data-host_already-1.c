/* Verify that we refuse 'acc_map_data' when the "host address [...] is already
   mapped".  */

/* { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } } */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <openacc.h>

int
main ()
{
  const int N = 101;

  char *h = (char *) malloc (N);
  assert (h);
  void *d1 = acc_malloc (N);
  assert (d1);
  acc_map_data (h, d1, N);

  void *d2 = acc_malloc (N);
  assert (d2);
  fprintf (stderr, "CheCKpOInT\n");
  acc_map_data (h, d2, N);

  return 0;
}


/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "host address \\\[\[0-9a-fA-FxX\]+, \\\+101\\\] is already mapped" } */
/* { dg-shouldfail "" } */
